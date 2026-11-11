{ inputs, ... }:

{
  perSystem = { lib, pkgs, ... }: {
    packages =
      let
        epkgs = pkgs.emacs.pkgs;
      in
      {
        startOllama = pkgs.writeShellScriptBin "start-ollama" ''
          if [[ "$(pidof ollama)" -gt 0 ]]; then
                  echo "ollama already running"
                else
                  ${lib.getExe pkgs.ollama} serve
                fi
        '';
        gptel-preset-collection = (
          epkgs.callPackage epkgs.trivialBuild rec {
            pname = "gptel-preset-collection";
            version = "current";
            src = inputs.gptel-preset-collection;

            propagatedUserEnvPkgs = with epkgs; [
              gptel
            ];

            buildInputs = propagatedUserEnvPkgs;
          }
        );
      };
  };

  flake.homeModules.ai =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    {
      home = {
        packages = [
          pkgs.ollama
          pkgs.jdt-language-server
        ];
        # file = {
        #   ".config/emacs/agents/bughunter.org" = ''
        #     
        #   '';
        # };
      };
      programs = {
        mcp.servers = {
          nixos = {
            command = "${pkgs.uv}/bin/uvx";
            args = [ "mcp-nixos" ];
          };
          lsp = {
            command = "npx";
            args = [
              "-y"
              "@theupsider/lsp-mcp@latest"
            ];
          };
	  jj = {
            command = "npx";
            args = [
              "-y"
              "jj-mcp-server"
            ];
          };
        } ;
        emacs.init = {
          ai = {
            copilot = {
              enable = true;
              keepOutOf = [
                "c-ts-mode"
                "json5-ts-mode"
                "json-ts-mode"
                "LaTeX-mode"
                "zenscript-mode"
              ];
            };
            gptel = {
              enable = true;
              enableMcpIntegration = true;
              introspection.enable = true;
              agent.enable = true;
            };
          };
          usePackage =
            let
              mkOllama = models: infix: {
                "gptel${infix}-model" = "'${lib.findFirst (x: true) "" models}";
                "gptel${infix}-backend" = ''
                  (gptel-make-ollama "Ollama"
                  	           :stream t
                  	           :protocol "http"
                  	           :host "localhost:11434"
                  	           :models '(${lib.concatMapStrings (k: "${k} ") models}))
                '';
              };
            in
            {
              gptel = {
                command = [ "start-ollama" ];
                generalOne.global-leader."gs" = '''("start" . start-ollama)'';
                setopt = {
                  gptel-max-tokens = 10000000;
                  gptel-prompt-prefix-alist = [
                    ''`(markdown-mode . ,(concat "meatbag ›  "))''
                    ''`(org-mode . ,(concat  "meatbag ›  "))''
                    ''`(text-mode . ,(concat "meatbag ›  "))''
                  ];
                  gptel-response-prefix-alist = [
                    '''(markdown-mode . "HK-47  ")''
                    '''(org-mode . "HK-47  ")''
                    '''(text-mode . "HK-47  ")''
                  ];
                  gptel-use-tools = "'force";
                }
                // mkOllama [ "qwen2.5:3b" "llama3.2:3b" ] "";
                generalOne.global-leader."gi" = "'gptel-inline";
                preface = ''
                  (defun start-ollama ()
                    (interactive)
                    (start-process-shell-command
                     "startOllama" nil "${pkgs.startOllama}/bin/start-ollama"))
                '';
                config = ''
                  (start-ollama)
                  (require 'gptel-agent)
                  (gptel-make-gh-copilot "copilot")
                  (gptel-make-openai "OpenRouter"
                    :host "openrouter.ai"
                    :endpoint "/api/v1/chat/completions"
                    :stream t
                    :key (lambda () (auth-source-pass-get 'secret "openrouter-api-key"))                   ;can be a function that returns the key
                    :models '(nvidia/nemotron-3.5-super-vl:free cohere/north-mini-code:free nvidia/nemotron-3-ultra-550b-a55b:free recursive/flint-v1.0:free xingchen-agi/xing4.0-29b-a4b:free liquid/lfm-2.5-2.6:free google/gemma-4-31b-it:free google/gemma-4-26b-a4b-it:free nvidia/nemotron-3-nano-omni-30b-a3b-reasoning:free))
                  (efs/evil-collection-remap
                   'evil-collection-gptel-setup
                   'normal
                   gptel-mode-map
                   "<return>"
                   'gptel-send
                   "RET"
                   'gptel-send)
                  (efs/evil-collection-remap
                   'evil-collection-gptel-setup
                   'insert
                   gptel-mode-map
                   "<return>"
                   'newline
                   "RET"
                   'newline)
                  
                  (gptel-make-preset
                      'translate
                    :system "Translate to english."
                    :model 'llama3.2:3b)
                  
                  (defun efs/translate (foreign-text)
                    "Translates FOREIGN-TEXT to English & outputs it in an org buffer called Translation."
                    (with-current-buffer (get-buffer-create "Translation")
                      (switch-to-buffer (current-buffer))
                      (org-mode)
                      (let ((gptel-model 'llama3.2:3b))
                        (gptel-request
                  	  (concat
                             foreign-text " Translate the following text to English.")))))
                  
                  (with-eval-after-load 'evil
                    (evil-ex-define-cmd
                     "translateb" (cmd! (efs/translate (buffer-string)))))
                  
                  (defun efs/semantic-search (term)
                    "Search for TERM in your notes."
                    (require 'org-semantic)
                    (apply 'concat
                           (mapcar
                            (lambda (entry)
                              (concat
                               (plist-get entry :heading)
                               "\n"
                               (plist-get entry :text)
                               "\n\n\n"))
                            (plist-get (org-semantic-search term) :hits))))
                  
                  (gptel-make-tool
                   :name "Denote"
                   :function 'efs/semantic-search
                   :description "Search the user's notes for a word or phrase. This tool will return parts of the notes that are a close match."
                   :args '((:name "term" :type "string" :description "The word or phrase to search for.")))
                  
                  
                  (gptel-make-preset
                      'denote
                    :system '(:append "Search the user's notes to learn about the relevant topic, then respond.")
                    :tools '(:append ("Denote")))
                  
                '';
              };

              gptel-agent.config = ''
                (gptel-make-preset
                    'web
                  :description "A preset for quick & cheap websearches."
                  :model 'llama3.2:3b
                  :system
                  '(:append
                    "Search the web to learn about the relevant topic, then respond.")
                  :tools '(:append ("WebSearch" "WebFetch")))
                
                (gptel-make-preset
                    'jarvis
                  :description "A preset for executing shell commands."
                  :model 'qwen2.5:3b
                  :system
                  '(:append
                    "You are a command runner. Your task is to execute shell commands to achieve the user's goal.
                Here are some tips:
                - When the user wants to regenerate their configs, run 'pkg rebuild'.
                - When the user wants to make a new VCs change, call 'jj new'. If the user says what the change is for, use 'jj new -m \"What the change is for\"'.
                - When the user wants to squash a VCs change, call 'jj squash'.
                - When the user wants to describe a VCs change, call 'jj describe'.")
                  :tools '(:append ("Bash")))
                
                (gptel-make-preset
                    'efs-agent
                  :description "An extended opencode agent with LSP support"
                  :parents '(gptel-agent)
                  :system '(:append "# Language Server Protocol
                - If possible, always init the LSP server for the language you're working in before doing any work.
                - If you must reformat a file & an lsp server is available, use the LSP server's formatting capabilities. Refrain from reformatting otherwise.
                - If an LSP server is available, use its search capabilities librerally when trying to understand the codebase.
                
                # Version Control
                - JJ is used as a version control system.
                - Before doing any work, use the new tool to create a new change.
                - After completing your work, use the describe tool to describe your change.
                ")
                  :pre (lambda () (gptel-mcp-connect '("lsp" "jj") 'sync))
                  :tools '(:append ("mcp-lsp" "mcp-jj")))
              '';

              gptel-quick.setopt = mkOllama [ "qwen2.5:3b" ] "-quick";

              gptel-inline = {
                enable = true;
                after = [ "gptel" ];
                config = ''
                  (defun gptel-inline--response-overlay-append-chunk (ov chunk)
                    "Append text CHUNK to the buffer-based response overlay OV.
                  Inserts into the source buffer and triggers font-lock."
                    (let* ((src-buf (plist-get (overlay-get ov 'gptel-inline) :src)))
                      (when (buffer-live-p src-buf)
                        (with-current-buffer src-buf
                          (let ((inhibit-read-only t))
                            (goto-char (point-max))
                            (insert chunk)
                            (font-lock-ensure))))))
                '';
                generalTwoConfig.":nm" = {
                  gptel-inline-map = {
                    "RET" = "'gptel-inline-send";
                    "H" = "'gptel-inline-help";
                    "C-a" = "'gptel-inline-append";
                    "C-b" = "'gptel-switch-buffer";
                    "C-SPC" = "'gptel-inline-cycle-reference";
                  };
                  gptel-inline--response-overlay-mode-map = {
                    "M-e" = "'gptel-inline--response-overlay-down";
                    "M-o" = "'gptel-inline--response-overlay-up";
                    "C-M-e" = "'gptel-inline--response-overlay-pagedown";
                    "C-M-o" = "'gptel-inline--response-overlay-pageup";
                    "C-d" = "'gptel-inline--response-overlay-dispatch";
                  };
                };
              };

              mcp.config = ''
                ;; Borrowed from Karthinks
                (gptel-make-preset
                    'nixos
                  :description "TOOLS: Add NixOS MCP"
                  :pre (lambda () (gptel-mcp-connect '("nixos") 'sync))
                  :system
                  '(:append
                    "\n\nUse the provided NixOS tools to look for up-to-date information and\
                 examine the state of my system")
                  :tools '(:append ("mcp-nixos")))
              '';

              gptel-preset-collection = {
                enable = true;
                after = [ "gptel" ];
              };

              popper.setopt.popper-reference-buffers = [
                ''"^\\*gptel-opencode-agent:.*\\*"''
                ''"^\\*gptel:.*\\*"''
                ''"^\\*Ollama\\*"''
                ''"^\\*copilot\\*"''
              ];
            };
        };
      };
    };
}
