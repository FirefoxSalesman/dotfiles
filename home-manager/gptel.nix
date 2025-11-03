{ config, pkgs, inputs, ... }:

{
  programs.emacs.init.usePackage = {
    gptel = {
      enable = true;
      defer = true;
      command = ["start-ollama"];
      setopt = {
	gptel-default-mode = "'org-mode";
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
	gptel-backend = ''
	  (gptel-make-ollama "Ollama"
	    :stream t
	    :protocol "http"
	    :host "localhost:11434"
	    :models '(qwen3:latest llama3.2:3b))
	'';
	# gptel-backend = ''(gptel-make-gh-copilot "Copilot")'';
      };
      generalOne.global-leader = {
	"g" = '''(:ignore t :which-key "gptel")'';
	"gs" = '''("start" . start-ollama)'';
	"gp" = '''("prompt" . gptel)'';
      };
      generalTwoConfig.":n".gptel-mode-map."S-RET" = "'gptel-menu";
      preface = ''
	(defun start-ollama ()
	  (interactive)
	  (start-process-shell-command "start-ollama" nil "${(import ./scripts/start-ollama.nix { inherit pkgs config; })}/bin/start-ollama"))
      '';
      config = "(start-ollama)";
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
  };
}
