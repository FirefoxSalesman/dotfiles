{ inputs, ... }:

{
  perSystem = { lib, pkgs, ... }: {
    packages = let
      epkgs = pkgs.emacs.pkgs;
    in {
      startOllama = pkgs.writeShellScriptBin "start-ollama" ''
	if [[ "$(pidof ollama)" -gt 0 ]]; then
          echo "ollama already running"
        else
          ${lib.getExe pkgs.ollama} serve
        fi
      '';
    };
  };

  flake.homeModules.ai = { lib, config, pkgs, ... }: {
    home.packages = [pkgs.ollama];
    programs.emacs.init = {
      ai = {
	copilot = {
	  enable = true;
	  keepOutOf = ["c-ts-mode" "json5-ts-mode" "json-ts-mode" "LaTeX-mode" "zenscript-mode"];
	};
	gptel = {
	  enable = true;
	  macher.enable = true;
	  introspection = {
	    enable = true;
	    model = "llama3-groq-tool-use:8b";
	  };
	};
      };
      usePackage = let
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
      in {
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
	  } // mkOllama ["llama3-groq-tool-use:8b" "llama3.2:3b" "llama3.2:1b"] "";
	  preface = ''
	    (defun start-ollama ()
	      (interactive)
	      (start-process-shell-command "startOllama" nil "${pkgs.startOllama}/bin/start-ollama"))
	  '';
	  config = ''
	    (start-ollama)
	    (gptel-make-gh-copilot "copilot")
	  '';
	};

	gptel-quick.setopt = mkOllama ["llama3.2:1b"] "-quick";

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
