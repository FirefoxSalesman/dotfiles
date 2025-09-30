{ config, pkgs, inputs, ... }:

{
  programs.emacs.init.usePackage = {
    gptel = {
      enable = true;
      defer = true;
      command = ["start-ollama"];
      custom = {
	gptel-default-mode = "'org-mode";
	gptel-max-tokens = 10000000;
	gptel-prompt-prefix-alist = ''
	  `((markdown-mode . ,(concat "meatbag ›  "))
            (org-mode . ,(concat  "meatbag ›  "))
            (text-mode . ,(concat "meatbag ›  ")))
	'';
	gptel-response-prefix-alist = ''
	  '((markdown-mode . "HK-47  ")
            (org-mode . "HK-47  ")
            (text-mode . "HK-47  "))
	'';
      };
      generalOne.global-leader = {
	"g" = '''(:ignore t :which-key "gptel")'';
	"gs" = '''(start-ollama :which-key "start")'';
	"gp" = '''(gptel :which-key "prompt")'';
      };
      generalTwoConfig.":n".gptel-mode-map."S-RET" = "'gptel-menu";
      preface = ''
	(defun start-ollama ()
	  (interactive)
	  (start-process-shell-command "start-ollama" nil "${(import ./scripts/start-ollama.nix { inherit pkgs config; })}/bin/start-ollama"))
      '';
      config = ''
	(start-ollama)
	
	(setopt gptel-backend (gptel-make-ollama "Ollama"
				:stream t
				:protocol "http"
				:host "localhost:11434"
				:models '(qwen3:latest llama3.2:3b)))
	
	;; (setopt gptel-backend (gptel-make-gh-copilot "Copilot"))
      '';
    };

    gptel-quick = {
      enable = true;
      defer = true;
      generalOne = {
	embark-general-map."?" = '''(gptel-quick :which-key "summarize")''; 
	global-leader."gq" = '''(gptel-quick :which-key "summarize")'';
      };
      config = ''
	(setopt gptel-quick-backend (gptel-make-ollama "Ollama"
				      :stream t
				      :protocol "http"
				      :host "localhost:11434"
				      :models '(qwen3:latest))
		gptel-quick-model 'qwen3:latest)
      '';
    };

    gptel-aibo = {
      enable = true;
      generalOne.":ie" = {
	"M-/" = "'gptel-aibo-complete-at-point";
	"M-?" = "'gptel-aibo-apply-last-suggestions";
      };
    };

    macher = {
      enable = true;
      command = ["macher-install"];
      generalOne.global-leader = {
	"gi" = "'macher-implement";
	"gr" = "'macher-revise";
	"gd" = "'macher-discuss";
	"ga" = "'macher-abort";
      };
    };

    ragmacs = {
      enable = false;
      after = ["gptel"];
      config = ''
	(gsetq gptel-tools (list ragmacs-eval
				 ragmacs-symbolp
				 ragmacs-load-paths
				 ragmacs-features
				 ragmacs-manuals
				 ragmacs-manual-nodes
				 ragmacs-manual-node-contents
				 ragmacs-featurep
				 ragmacs-library-source
				 ragmacs-symbol-manual-node
				 ragmacs-function-source
				 ragmacs-variable-source
				 ragmacs-variable-value
				 ragmacs-function-docstring
				 ragmacs-variable-docstring
				 ragmacs-function-completions
				 ragmacs-command-completions
				 ragmacs-variable-completions
				 ragmacs-simulate-error
				 ragmacs-coerce-nil
				 ragmacs-all-arg-types
				 ragmacs-async))
      '';
    };
  };
}
