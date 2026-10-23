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
      home.packages = [ pkgs.ollama ];
      programs = {
        mcp.servers.nixos = {
          command = "${pkgs.uv}/bin/uvx";
          args = [ "mcp-nixos" ];
        };
        emacs.init = {
          ai = {
            copilot = {
              enable = false;
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
              introspection = {
                enable = true;
                model = "qwen3:8b";
              };
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
                // mkOllama [ "llama3.2:3b" "qwen2.5-coder:7b" "qwen3:8b" "llama3.2:1b" ] "";
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
                   :model 'llama3.2:1b)
                  
                  (defun efs/translate (foreign-text)
                    "Translates FOREIGN-TEXT to English & outputs it in an org buffer called Translation."
                    (with-current-buffer (get-buffer-create "Translation")
                      (switch-to-buffer (current-buffer))
                      (org-mode)
                      (let ((gptel-model 'llama3.2:1b))
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
                   :args '((:name "term" :type "string" :description "The word or phrase to search for."))
                   :include t)
                  
                  (gptel-make-preset
                      'denote
                    :system '(:append "Search the user's notes to learn about the relevant topic, then respond.")
                    :tools '(:append ("Denote")))
                  
                  (gptel-make-preset
                      'web
                    :system '(:append "Search the web to learn about the relevant topic, then respond.")
                    :tools '(:append ("WebSearch" "WebFetch")))
                '';
              };

              gptel-quick.setopt = mkOllama [ "llama3.2:1b" ] "-quick";

              gptel-inline = {
                enable = true;
                after = [ "gptel" ];
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
                 :tools '(:append ("mcp-nixos"))
                 :model 'qwen3:8b
                 :backend "Ollama")
              '';

              gptel-preset-collection = {
                enable = true;
                after = [ "gptel" ];
              };

              org-semantic = {
                enable = true;
                setopt.org-semantic-vault-root = ''"~/doc/denote"'';
                generalOne.global-leader = {
                  oS = '''(:ignore t :which-key "semantic")'';
                  oSf = "'org-semantic-find";
                  oSp = "'org-semantic-find-at-point";
                  oSr = "'org-semantic-reindex";
                };
                config = "(org-semantic-auto-reindex-mode)";
              };

              aidermacs = {
                enable = false;
                extraPackages = [ pkgs.aider-chat ];
                generalOne.global-leader."gA" = "'aidermacs-transient-menu";
                config = "(start-ollama)";
                setopt = {
                  aidermacs-default-model = ''"ollama/qwen3:8b"'';
                  aidermacs-extra-args = [ ''"--no-git"'' ];
                };
              };

              popper.setopt.popper-reference-buffers = [
                "'aidermacs-comint-mode"
                ''"^\\*gptel-inline\\*"''
                ''"^\\*Ollama\\*"''
                ''"^\\*copilot\\*"''
              ];
            };
        };
      };
    };
}
