{ inputs, ... }:

{
  perSystem = { lib, pkgs, ... }: {
    packages =
      let epkgs = pkgs.emacs.pkgs;
      in {
	startOllama = pkgs.writeShellScriptBin "start-ollama" ''
	  if [[ "$(pidof ollama)" -gt 0 ]]; then
          echo "ollama already running"
        else
          ${lib.getExe pkgs.ollama} serve
        fi
	'';
	gptel-quick = (epkgs.callPackage
	  epkgs.trivialBuild rec {
	  pname = "gptel-quick";
	  version = "current";
	  src = inputs.gptel-quick;

	  propagatedUserEnvPkgs = with epkgs; [
	    gptel
	  ];

	  buildInputs = propagatedUserEnvPkgs;
	  }
	);
      };
  };

  flake.homeModules.ai = { config, pkgs, ... }: {
    home.packages = [pkgs.ollama];
    programs.emacs.init = {
      ai.copilot = {
	enable = true;
	keepOutOf = ["c-ts-mode" "json5-ts-mode" "json-ts-mode" "LaTeX-mode" "zenscript-mode"];
      };
      usePackage = {
	gptel = {
	  enable = true;
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
	        :models '(qwen3.5:4b))
	    '';
	    # gptel-backend = ''(gptel-make-gh-copilot "Copilot")'';
	  };
	  generalOne.global-leader = {
	    "g" = '''(:ignore t :which-key "gptel")'';
	    "gs" = '''("start" . start-ollama)'';
	    "gp" = '''("prompt" . gptel)'';
	    "gt" = '''("add text to context" . gptel-add)'';
	    "gf" = '''("add file to context" . gptel-add-file)'';
	    "gm" = '''("open configuration menu" . gptel-menu)'';
	    "gr" = '''("rewrite current region" . gptel-rewrite)'';
	  };
	  preface = ''
	    (defun start-ollama ()
	      (interactive)
	      (start-process-shell-command "startOllama" nil "${pkgs.startOllama}/bin/start-ollama"))
	  '';
	  config = "(start-ollama)";
	};

	gptel-org = {
	  enable = true;
	  package = epkgs: epkgs.gptel;
	  generalOne.global-leader = {
	    "go" = '''("limit context to current org heading" . gptel-org-set-topic)'';
	    "gO" = '''("store gptel config as org properties" . gptel-org-set-properties)'';
	  };
	};

	gptel-quick = {
	  enable = true;
	  generalOne.global-leader."ge" = '''("Explain the current region" . gptel-quick)'';
	  setopt = {
	    gptel-model = "'llama3.2:1b" ;
	    gptel-backend = ''
	      (gptel-make-ollama "Ollama"
	        :stream t
	        :protocol "http"
	        :host "localhost:11434"
	        :models '(llama3.2:1b))
	    '';
	  };
	};

	gptel-magit = {
	  enable = true;
	  after = ["magit"];
	  ghookf = ["('magit-mode 'gptel-magit-install)"];
	};

	ob-gptel = {
	  enable = true;
	  config = ''
	    (defun ob-gptel-setup-completions ()
              (add-hook 'completion-at-point-functions
                'ob-gptel-capf nil t))
	  '';
	  hook = ["(org-mode . ob-gptel-setup-completions)"];
	  babel = "gptel";
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
