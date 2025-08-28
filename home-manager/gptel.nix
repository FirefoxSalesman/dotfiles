  { config, pkgs, inputs, ... }:

  {
    programs.emacs.init.usePackage = {
      gptel = {
        enable = true;
        defer = true;
        command = ["start-ollama"];
        custom.gptel-default-mode = "'org-mode";
        generalOne.global-leader = {
          "g" = '''(:ignore t :which-key "gptel")'';
          "gs" = '''(start-ollama :which-key "start")'';
          "gp" = '''(gptel :which-key "prompt")'';
        };
        generalTwo."local-leader".gptel-mode-map = {
          "d" = '''(gptel-send :which-key "send")'';
          "m" = '''(gptel-menu :which-key "menu")'';
        };
        config = ''
            (gsetq gptel-backend (gptel-make-ollama "Ollama"
            		       :stream t
            		       :protocol "http"
            		       :host "localhost:11434"
            		       :models '(llama3.2:latest))
                   gptel-max-tokens 10000000
                   gptel-prompt-prefix-alist '((default . "You are a large language model and a helpful assistant. Respond concisely.")
            				   (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
            				   (writing . "You are a large language model and a writing assistant. Respond concisely.")
            				   (chat . "You are a large language model and a conversation partner. Respond concisely.")))
          
            (defun start-ollama ()
              (interactive)
              (start-process-shell-command "start-ollama" nil "${(import ./scripts/start-ollama.nix { inherit pkgs config; })}/bin/start-ollama"))
        '';
      };

      gptel-quick = {
        enable = true;
        defer = true;
        generalOne = {
          embark-general-map."?" = '''(gptel-quick :which-key "summarize")''; 
          global-leader."gq" = '''(gptel-quick :which-key "summarize")'';
        };
      };
    };
  }
