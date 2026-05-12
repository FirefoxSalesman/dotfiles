{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.qutebrowser-emacs = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "qutebrowser";
	version = "current";
	src = inputs.exwm-qutebrowser;

	propagatedUserEnvPkgs = with epkgs; [
	  consult
	  exwm
	  password-store
	  dash
	  evil
	  password-store-otp
	  doom-modeline
	];

	buildInputs = propagatedUserEnvPkgs;
      }
    );
  };

  flake.homeModules.web = { ... }: {
    programs = {
      emacs.init.usePackage = {
	qutebrowser = {
	  enable = true;
	  ghookf = [
	    "('exwm-init 'global-qutebrowser-exwm-mode)"
	  ];
	  gfhookf = [
	    "('qutebrowser-exwm-mode 'evil-normal-state)"
	  ];
	};
	qutebrowser-evil = {
	  enable = true;
	  package = epkgs: epkgs.qutebrowser;
	  ghookf = [
	    "('global-qutebrowser-exwm-mode 'qutebrowser-evil-state-mode)"
	  ];
	};
	qutebrowser-fileselect = {
	  enable = true;
	  package = epkgs: epkgs.qutebrowser;
	  ghookf = [
	    "('global-qutebrowser-exwm-mode 'qutebrowser-fileselect-mode)"
	  ];
	  setopt.qutebrowser-fileselect-handler = "'qutebrowser-fileselect-completion";
	};
      };

      qutebrowser.settings = {
	tabs = {
	  tabs_are_windows = true;
	  show = "never";
	};
	window.title_format = "{current_title}";
	new_instance_open_target = "tab-silent";
	statusbar.show = "never";
	fileselect = {
	  handler = "external";
	  single_file.command = [
	    "emacsclient"
	    "{}"
	  ];
	  folder.command = [
	    "emacsclient"
	    "{}"
	  ];
	  multiple_files.command = [
	    "emacsclient"
	    "{}"
	  ];
	};
      };
    };
  };
}
