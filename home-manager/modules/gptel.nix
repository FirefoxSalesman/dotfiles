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
      ai = {
	copilot = {
	  enable = true;
	  keepOutOf = ["c-ts-mode" "json5-ts-mode" "json-ts-mode" "LaTeX-mode" "zenscript-mode"];
	};
	gptel.enable = true;
      };
      usePackage = {
	gptel = {
	  command = ["start-ollama"];
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
	    gptel-model = "'qwen3.5-coder:4b";
	    gptel-backend = ''
	      (gptel-make-ollama "Ollama"
	        :stream t
	        :protocol "http"
	        :host "localhost:11434"
	        :models '(qwen3.5:4b))
	    '';
	    # gptel-backend = ''(gptel-make-gh-copilot "Copilot")'';
	  };
	  preface = ''
	    (defun start-ollama ()
	      (interactive)
	      (start-process-shell-command "startOllama" nil "${pkgs.startOllama}/bin/start-ollama"))
	  '';
	  config = "(start-ollama)";
	};

	gptel-quick.setopt = {
	  gptel-model = "'llama3.2:1b" ;
	  gptel-backend = ''
	    (gptel-make-ollama "Ollama"
	        :stream t
	        :protocol "http"
	        :host "localhost:11434"
	        :models '(llama3.2:1b))
	  '';
	};

	macher = {
	  enable = true;
	  command = ["macher-install"];
	  generalOne.global-leader = {
	    "ga" = '''(:ignore t :which-key "agent")'';
	    "gai" = "'macher-implement";
	    "gar" = "'macher-revise";
	    "gad" = "'macher-discuss";
	    "gaa" = "'macher-abort";
	  };
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
