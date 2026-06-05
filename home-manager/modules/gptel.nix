{ inputs, ... }:

{
  perSystem = { lib, pkgs, ... }: {
    packages.startOllama = pkgs.writeShellScriptBin "start-ollama" ''
      if [[ "$(pidof ollama)" -gt 0 ]]; then
        echo "ollama already running"
      else
        ${lib.getExe pkgs.ollama} serve
      fi
    '';
  };

  flake.homeModules.ai = { config, pkgs, ... }: {
    home.packages = [pkgs.ollama];
    programs.emacs.init = {
      # ide.copilot = {
      # 	enable = true;
      # 	keepOutOf = ["c-ts-mode" "json5-ts-mode" "json-ts-mode" "LaTeX-mode" "zenscript-mode"];
      # };
      usePackage = {
	minuet = {
	  enable = true;
	  ghookf = ["('prog-mode 'minuet-auto-suggestion-mode)"];
	  generalOneConfig.minuet-active-mode-map = {
	    "M-e" = "'minuet-next-suggestion";
	    "M-o" = "'minuet-previous-suggestion";
	    "C-i" = "'minuet-accept-suggestion";
	    "M-i" = "'minuet-accept-suggestion-line";
	    "M-d" = "'minuet-dismiss-suggestion";
	  };
	  setopt = {
	    minuet-provider = "'openai-fim-compatible";
	    minuet-n-completions = 1;
	    minuet-context-window = 512;
	  };
	  config = ''
	    (start-ollama)
	    (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
	    (plist-put minuet-openai-fim-compatible-options :name "Ollama")
	    (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
	    (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")
	    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)
	  '';
	};
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
	    gptel-model = "'qwen3.5-coder:4b";
	    gptel-backend = ''
	      (gptel-make-ollama "Ollama"
	        :stream t
	        :protocol "http"
	        :host "localhost:11434"
	        :models '(qwen2.5-coder:3b qwen3.5:4b))
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
	      (start-process-shell-command "startOllama" nil "${pkgs.startOllama}/bin/start-ollama"))
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

	gptel-magit = {
	  enable = true;
	  ghookf = ["('magit-mode 'gptel-magit-install)"];
	};

	# mcp = {
	#   enable = true;
	#   after = ["gptel"];
	#   config = ''
	# 	(require 'mcp-hub)
	#     (require 'gptel-integrations)
	#   '';
	# };
      };
    };
  } ;
}
