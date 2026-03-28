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
	    "('server-visit 'qute/dired-hook)"
	    "('qutebrowser-exwm-mode 'evil-normal-state)"
	  ];
	  config = ''
	    (define-minor-mode qute-dired-mode
	      "Used for dired buffers qutebrowser is using as a file picker"
	      :keymap '())
	    
	    (defun qute/choose-file ()
	      (interactive)
	      (let ((files (dired-get-marked-files)))
	        (with-temp-file qute-filename
	          (insert (s-join "\n" files)))
	        (remove-hook 'dired-mode-hook 'qute-dired-mode)
	        (dolist (buffer dired-buffers) (when qute-dired-mode (kill-buffer (cdr buffer))))))
	    
	    (defun qute/dired-hook (&optional _)
	      (when (s-starts-with? "/tmp/qutebrowser-fileselect" buffer-file-name)
	        (setq qute-filename buffer-file-name)
	        (kill-buffer)
	        (add-hook 'dired-mode-hook 'qute-dired-mode)
	        (setq qute-dired-buffers (list (dired "~/")))))
	  '';
	  generalOneConfig.qute-dired-mode-map."C-c C-c" = "#'qute/choose-file";
	  generalTwoConfig.":n".qute-dired-mode-map = {
	    "i" = "'dired-find-file";
	    "n" = "'dired-up-directory";
	  };
	};
	qutebrowser-evil = {
	  enable = true;
	  package = epkgs: epkgs.qutebrowser;
	  ghookf = [
	    "('global-qutebrowser-exwm-mode 'qutebrowser-evil-state-mode)"
	  ];
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
