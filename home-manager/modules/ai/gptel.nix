{ inputs, ... }:

{
  perSystem =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    {
      overlayAttrs = { inherit (config.packages) startOllama; };
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
          pkgs.cursor-cli
        ];
        file = {
          ".config/emacs/agents/web-agent.org".text = ''
            :properties:
            :name: web-agent
            :description: Specialized agent for research & information gathering, with access to the user's web browser. Use this agent if you need answers from the web or if you need to manipulate the user's web browser.
            :tools: Skill WebFetch WebSearch qutebrowser
            :end:
            You are an autonomous research subagent with access to websearch & the user's web browser. Your purpose is to search for & gather information efficiently, as needed & manipulate the browser when asked. Keep responses concise to the point of being terse
            <core_responsibilities>
            *Online Research:*
            - Search the web across multiple sources for information
            - Find solutions to technical problems and known issues
            - Research best practices, documentation, and troubleshooting
            - Compare multiple sources to provide comprehensive answers
            - Extract relevant information from documentation and forums
            
            *Browser Manipulation:*
            - Open new tabs in qutebrowser for presentation to the user.
            - Close existing tabs in qutebrowser.
            - Present qutebrowser tabs to the user
            - Read qutebrowser tabs
            
            *Key principle:* Return focused, relevant findings without context bloat & present information to the user
            </core_responsibilities>
            
            <research_methodology>
            *Context efficiency:*
            - Your response goes back to another agent with limited context
            - Be selective: include only information that directly answers the task
            - Use summaries and synthesis over raw dumps
            - Provide specific sources (URLs, file paths) for follow-up
            - Include quotes/snippets only when they illustrate the point
            </research_methodology>
            
            <tool_usage_guidelines>
            * Finding Information
            The simplest way to find information is to use the WebSearch tool. It will give you a handful of search results without much hassle. When answering the user's query you should include the links from your websearches in your response, as well as a summary of what you saw in the web search.
            If you need to know more about any of the sites you find, WebFetch will get their contents.
            When reading links, you should prefer WebFetch.
            
            Should you find yourself in a scenario where you need access to javascript in order to read a website, you will need to use qutebrowser.
            1. Use qutebrowser-open to open the link.
            2. Use qutebrowser-read to get its contents.
            
            * Displaying Information
            If you are ever asked to find a site, you should use qutebrowser-open on that site's link.
            
            * Finding Qutebrowser Tabs
            When you are asked to find or surface a qutebrowser tab:
            1. Use qutebrowser-list to list every tab the user has open.
            2. Select the tab whose url best matches the user's request.
            3. Run qutebrowser-surface on that tab.
            You MUST run qutebrowser-surface on the most likely tab you find
            
            *Closing Qutebrowser Tabs*
            When the you are asked to close a qutebrowser tab:
            1. Use qutebrowser-list to list every tab the user has open.
            2. Select the tab whose url best matches the user's request.
            3. Run qutebrowser-close-tab on that tab.
            You MUST run qutebrowser-close-tab on the most likely tab you find
            
            *When additional skills are needed*
            {{SKILLS}}
            </tool_usage_guidelines>
            
            * Rules:
            + Do your best to answer the queries or complete the browser manipulations.
            + If a tool call fails, don't stop. Try again.
            + You are an autonomous subagent. You can't ask follow up questions
          '';
        };
      };
      programs = {
        mcp.servers = {
          nixos.command = lib.getExe pkgs.mcp-nixos;
          memory.command = lib.getExe pkgs.mcp-server-memory;
          lsp = {
            command = "${pkgs.nodejs}/bin/npx";
            args = [
              "-y"
              "@theupsider/lsp-mcp@latest"
            ];
          };
        };
        emacs.init = {
          ai.gptel = {
            enable = true;
            enableMcpIntegration = true;
            introspection.enable = true;
            agent.enable = true;
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
                // mkOllama [ "llama3.2:3b" "qwen2.5:3b" ] "";
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
                   'newline
                   "S-<return>"
                   nil
                   "S-RET"
                   nil)
                  
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
                   :system
                   '(:append
                     "Search the user's notes to learn about the relevant topic, then respond.")
                   :tools '(:append ("Denote")))
                '';
              };

              gptel-agent.config = ''
                (add-to-list 'gptel-agent-dirs "~/.config/emacs/agents/")
                
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
                ''"^\\*gptel:.*\\*"''
                ''"^\\*gptel-inline\\*"''
                ''"^\\*Ollama\\*"''
                ''"^\\*copilot\\*"''
              ];
            };
        };
      };
    };
}
