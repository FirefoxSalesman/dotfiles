{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages = {
      roll = (epkgs.callPackage
	epkgs.trivialBuild rec {
	pname = "roll";
	version = "current";
	src = inputs.roll;
	}
      );
      pertab = (epkgs.callPackage
	epkgs.trivialBuild rec {
	pname = "pertab";
	version = "current";
	src = inputs.pertab;

	propagatedUserEnvPkgs = with epkgs; [
	  elwm
	];

	buildInputs = propagatedUserEnvPkgs;
	}
      );
    };
  };

  flake.homeModules.exwm = { ... }:
  {
    programs.emacs.init.usePackage = {
      tab-bar = {
	enable = true;
	config = ''
	  (general-add-advice 'tab-new :after #'dashboard-open)
	  (defun efs/tab-bar-select ()
	     (interactive)
	     (setq tab-bar-tab-hints t)
	     (tab-bar-select-tab (string-to-number (read-string "Tab Number: ")))
	     (setq tab-bar-tab-hints nil))
	'';
	ghookf = ["('exwm-init 'tab-bar-mode)"];
	general."s-u" = "'tab-bar-hydra/body";
	setopt.tab-bar-select-restore-windows = false;
	extraConfig = ''
	  :pretty-hydra
            ((:color amaranth)
             ("Navigation"
              (("e" #'evil-tab-next "next")
               ("o" #'tab-bar-switch-to-prev-tab "prev")
               ("v" #'tab-recent "recent")
               ("b" #'tab-bar-lost-commands-switch-to-first-tab "first")
               ("B" #'tab-bar-lost-commands-switch-to-last-tab "last")
               ("/" #'efs/tab-bar-select "search"))
              "Creation/Deletion"
              (("s" #'tab-new "new")
               ("k" #'tab-close "close")
               ("r" #'tab-rename "rename")
               ("j" #'tab-undo "undo"))
              "Groups"
              (("g" #'tab-group "add to group")
               ("K" #'tab-close-group "close group"))
              "Organization"
              (("E" #'tab-bar-lost-commands-move-tab-forward "forward")
               ("O" #'tab-bar-lost-commands-move-tab-backward "backward"))
              "Exit"
              (("<return>" nil "" :color blue)
               ("<escape>" nil "" :color blue))))
	'';
      };

      tab-bar-lost-commands = {
	enable = true;
	command = [
	  "tab-bar-lost-commands-move-tab-forward"
	  "tab-bar-lost-commands-move-tab-backward"
	  "tab-bar-lost-commands-switch-to-first-tab"
	  "tab-bar-lost-commands-switch-to-last-tab"
	];
      };

      pertab = {
	enable = true;
	ghookf = ["('tab-bar-mode 'pertab-mode)"];
	setopt = {
	  pertab-default-layout  = "'master-stack";
	  pertab-next-buffer-function = "'bufler-cycle-buffers-forward";
	  pertab-previous-buffer-function = "'bufler-cycle-buffers-backward";
	};
	config = ''
	  (defvar pertab-scroll-enter-hook nil "Hook run when entering scrolling layout.")
	  (defvar pertab-scroll-exit-hook nil "Hook run when exiting scrolling layout.")
	  
	  (defun pertab-scroll-enter ()
	    "Set up the scrolling layout."
	    (roll-mode +1)
	    (run-hooks 'pertab-scroll-enter-hook))
	  
	  (defun pertab-scroll-exit ()
	    "Tear down the scrolling layout."
	    (roll-mode -1)
	    (pertab-set-tab-local 'roll-max-visible-panes roll-max-visible-panes)
	    (pertab-set-tab-local 'roll--panes roll--panes)
	    (pertab-set-tab-local 'roll--windows roll--windows)
	    (pertab-set-tab-local 'roll--nof-visible-panes roll--nof-visible-panes)
	    (pertab-set-tab-local 'roll--first-visible-pane roll--first-visible-pane)
	    (run-hooks 'pertab-scroll-exit-hook))
	  
	  (pertab-register-layout 'scroll '((roll-max-visible-panes . 2)
	  				  (roll--windows . ())
	  				  (roll--panes . ())
	  				  (roll--nof-visible-panes . 1)
	  				  (roll--first-visible-pane . 0))
	  			(pertab-layout-manager :lighter "[>]"
	  					       :enter-fun 'pertab-scroll-enter
	  					       :exit-fun 'pertab-scroll-exit
	  					       :horiz-split-fun 'roll-open
	  					       :vert-split-fun 'roll-open
	  					       :focus-left-fun 'roll-go-left
	  					       :focus-right-fun 'roll-go-right
	  					       :move-left-fun 'roll-move-left
	  					       :move-right-fun 'roll-move-right
	  					       :close-window-fun 'roll-close))
	  
	  (add-hook 'pertab-scroll-enter-hook (lambda () (golden-ratio-mode -1) (exwm-mff-mode +1)))
	  (add-hook 'pertab-scroll-exit-hook (lambda () (golden-ratio-mode +1) (exwm-mff-mode -1)))
	'';
      };

      pertab-monocle = {
	enable = true;
	package = epkgs: epkgs.pertab;
	after = ["pertab"];
      };

      pertab-follow = {
	enable = true;
	package = epkgs: epkgs.pertab;
	after = ["pertab"];
	gfhookf = [
	  "('pertab-follow-enter (lambda () (golden-ratio-mode -1)))"
	  "('pertab-follow-exit (lambda () (golden-ratio-mode +1)))"
	];
      };

      pertab-master-stack = {
	enable = true;
	package = epkgs: epkgs.pertab;
	after = ["pertab"];
      };

      pertab-manual = {
	enable = true;
	package = epkgs: epkgs.pertab;
	after = ["pertab"];
      };

      roll = {
	enable = true;
	setopt.roll-debug-enabled = false;
	config = ''
	  (defun roll--enable ()
             "Initialize Roll mode by setting up the initial state.
              Removes all other windows and creates the initial pane configuration."
             (if (equal roll--windows '())
	          (progn
		    (delete-other-windows)
	            (setq roll--windows (list (selected-window))))
                  (balance-windows))
             (when (equal roll--panes '()) (setq roll--panes (list (roll--make-snapshot))))
             (roll--debug "roll-mode enabled"))
	'';
      };
    };
  };
}
