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
               ("u" #'tab-undo "undo"))
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
	  (defvar pertab-follow--old-window-state nil "Window state prior to entering follow layout.")
	  (defvar pertab-follow--splits 1 "The number of splits to use in follow layout.")
	  (defvar pertab-follow-enter-hook nil "Hook run when entering follow layout.")
	  (defvar pertab-follow-exit-hook nil "Hook run when exiting follow layout.")
	  
	  
	  (defun pertab-follow-enter ()
	    "Sets up the follow layout."
	    (setq pertab-follow--old-window-state (current-window-configuration))
	    (delete-other-windows)
	    (follow-mode +1)
	    (dotimes (i pertab-follow--splits) (split-window-horizontally))
	    (run-hooks 'pertab-follow-enter-hook))
	  
	  (defun pertab-follow-exit ()
	    "Tears down follow layout."
	    (follow-mode -1)
	    (set-window-configuration pertab-follow--old-window-state)
	    (run-hooks 'pertab-follow-exit-hook))
	  
	  (defun pertab-follow-close ()
	    "Close the current window."
	    (setq pertab-follow--splits (max 0 (- pertab-follow--splits 1)))
	    (pertab-set-tab-local 'pertab-follow--splits pertab-follow--splits)
	    (delete-window))
	  
	  (defun pertab-follow-split ()
	    "Split the current window."
	    (setq pertab-follow--splits (+ 1 pertab-follow--splits))
	    (pertab-set-tab-local 'pertab-follow--splits pertab-follow--splits)
	    (split-window-horizontally))
	  
	  (pertab-register-layout 'follow '((pertab-follow--splits . 1)) (pertab-layout-manager :lighter "|||"
	  										      :enter-fun 'pertab-follow-enter
	  										      :exit-fun 'pertab-follow-exit
	  										      :focus-left-fun 'windmove-left
	  										      :focus-right-fun 'windmove-right
	  										      :close-window-fun 'pertab-follow-close
	  										      :horiz-split-fun 'pertab-follow-split
	  										      :vert-split-fun 'pertab-follow-split))
	  (defvar pertab-master-stack-enter-hook nil "Hook run when entering master/stack layout.")
	  (defvar pertab-master-stack-exit-hook nil "Hook run when exiting master/stack layout.")
	  
	  (defun pertab-master-stack-enter ()
	    "Set up the master/stack layout."
	    (setq elwm-current-layout 'tile-vertical-left)
	    (pertab-set-tab-local 'elwm-current-layout 'tile-vertical-left)
	    (run-hooks 'pertab-master-stack-enter-hook))
	  
	  (defun pertab-master-stack-exit ()
	    "Tear down the master/stack layout."
	    (run-hooks 'pertab-master-stack-exit-hook))
	  
	  (defun pertab-master-stack-deactivate ()
	    "Move to the previous window."
	    (elwm-activate-window (prefix-numeric-value -1)))
	  
	  (defun pertab-master-stack-derotate ()
	    "Move the windows backwards."
	    (elwm-rotate-window (prefix-numeric-value -1)))
	  
	  (defun pertab-rotate-windows ()
	    "Move the windows forwards."
	    (elwm-rotate-window 1))
	  
	  (defun pertab-master-stack-remove-window ()
	    "Close the current window."
	    (when (elwm--in-master-area-p)
	      (elwm-rotate-window 1)
	      (elwm-activate-window))
	    (delete-window))
	  
	  (pertab-register-layout 'master-stack '((elwm-current-layout . 'tile-vertical-left))
	  			(pertab-layout-manager :lighter "[]="
	  					       :enter-fun 'pertab-master-stack-enter
	  					       :exit-fun 'pertab-master-stack-exit
	  					       :horiz-split-fun 'elwm-split-window
	  					       :vert-split-fun 'elwm-split-window
	  					       :focus-down-fun 'elwm-activate-window
	  					       :focus-up-fun 'pertab-master-stack-deactivate
	  					       :move-down-fun 'pertab-rotate-windows
	  					       :move-up-fun 'pertab-master-stack-derotate
	  					       :close-window-fun 'pertab-master-stack-remove-window))
	  
	  (defvar pertab-master-stack-horizontal-enter-hook nil "Hook run when entering horizontal master/stack layout.")
	  (defvar pertab-master-stack-horizontal-exit-hook nil "Hook run when exiting horizontal master/stack layout.")
	  
	  (defun pertab-master-stack-horizontal-enter ()
	    "Set up the horizontal master/stack layout."
	    (setq elwm-current-layout 'tile-horizontal-top)
	    (pertab-set-tab-local 'elwm-current-layout 'tile-horizontal-top)
	    (run-hooks 'pertab-master-stack-horizontal-enter-hook))
	  
	  (defun pertab-master-stack-horizontal-exit ()
	    "Tear down the horizontal master/stack layout."
	    (run-hooks 'pertab-master-stack-horizontal-exit-hook))
	  
	  (pertab-register-layout 'master-stack-horizontal '()
	  			(pertab-layout-manager :lighter "|-|"
	  					       :enter-fun 'pertab-master-stack-horizontal-enter
	  					       :exit-fun 'pertab-master-stack-horizontal-exit
	  					       :horiz-split-fun 'elwm-split-window
	  					       :vert-split-fun 'elwm-split-window
	  					       :focus-down-fun 'elwm-activate-window
	  					       :focus-up-fun 'pertab-master-stack-deactivate
	  					       :move-down-fun 'pertab-rotate-windows
	  					       :move-up-fun 'pertab-master-stack-derotate
	  					       :close-window-fun 'pertab-master-stack-remove-window))
	  
	  (defvar pertab-manual-enter-hook nil "Hook run when entering manual layout.")
	  (defvar pertab-manual-exit-hook nil "Hook run when exiting manual layout.")
	  
	  (defun pertab-manual-enter ()
	    "Set up the manual layout."
	    (run-hooks 'pertab-manual-stack-enter-hook))
	  
	  (defun pertab-manual-exit ()
	    "Tear down the manual layout."
	    (run-hooks 'pertab-manual-stack-exit-hook))
	  
	  (pertab-register-layout 'manual '()
	  			(pertab-layout-manager :lighter "[+]"
	  					       :enter-fun 'pertab-manual-enter
	  					       :exit-fun 'pertab-manual-exit
	  					       :horiz-split-fun 'split-window-horizontally
	  					       :vert-split-fun 'split-window-vertically
	  					       :focus-left-fun 'windmove-left
	  					       :focus-right-fun 'windmove-right
	  					       :focus-down-fun 'windmove-down
	  					       :focus-up-fun 'windmove-up
	  					       :move-left-fun 'windmove-swap-states-left
	  					       :move-right-fun 'windmove-swap-states-right
	  					       :move-down-fun 'windmove-swap-states-down
	  					       :move-up-fun 'windmove-swap-states-up
	  					       :close-window-fun 'delete-window))
	  
	  (defvar pertab-scroll-enter-hook nil "Hook run when entering scrolling layout.")
	  (defvar pertab-scroll-exit-hook nil "Hook run when exiting scrolling layout.")
	  
	  (defun pertab-scroll-enter ()
	    "Set up the scrolling layout."
	    (roll-mode +1)
	    (run-hooks 'pertab-scroll-stack-enter-hook))
	  
	  (defun pertab-scroll-exit ()
	    "Tear down the scrolling layout."
	    (roll-mode -1)
	    (pertab-set-tab-local 'roll-max-visible-panes roll-max-visible-panes)
	    (pertab-set-tab-local 'roll--panes roll--panes)
	    (pertab-set-tab-local 'roll--windows roll--windows)
	    (pertab-set-tab-local 'roll--nof-visible-panes roll--nof-visible-panes)
	    (pertab-set-tab-local 'roll--first-visible-pane roll--first-visible-pane)
	    (run-hooks 'pertab-scroll-stack-exit-hook))
	  
	  (pertab-register-layout 'scroll '((roll-max-visible-panes . 2)
	  				  (roll--windows . ())
	  				  (roll--panes . ())
	  				  (roll--nof-visible-panes . 1)
	  				  (roll--first-visible-pane . 0))
	  			(pertab-layout-manager :lighter "[]>"
	  					       :enter-fun 'pertab-scroll-enter
	  					       :exit-fun 'pertab-scroll-exit
	  					       :horiz-split-fun 'roll-open
	  					       :vert-split-fun 'roll-open
	  					       :focus-left-fun 'roll-go-left
	  					       :focus-right-fun 'roll-go-right
	  					       :move-left-fun 'roll-move-left
	  					       :move-right-fun 'roll-move-right
	  					       :close-window-fun 'roll-close))
	  
	  (add-hook 'pertab-follow-enter-hook (lambda () (golden-ratio-mode -1)))
	  (add-hook 'pertab-follow-exit-hook (lambda () (golden-ratio-mode +1)))
	'';
      };

      pertab-monocle = {
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
             ;;(delete-other-windows)
             (when (equal roll--windows '())
		   (delete-other-windows)
	           (setq roll--windows (list (selected-window))))
             ;;(setq roll--nof-visible-panes 1)
             ;;(setq roll--first-visible-pane 0)
             (when (equal roll--panes '()) (setq roll--panes (list (roll--make-snapshot))))
             (roll--debug "roll-mode enabled"))
	'';
      };
    };
  };
}
