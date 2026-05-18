{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.roll = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "roll";
	version = "current";
	src = inputs.roll;
      }
    );
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
	  
	  (defvar pertab-default-layout nil "The default layout to use when opening a new tab.")
	  (defvar pertab--default-tab-local-variables '() "The default values for each tab-local variable.")
	  (defvar pertab--layout-registry '() "An alist of symbols to their 'pertab-layout-manager' objects.")
	  (defvar pertab-next-buffer-function 'next-buffer "The function used to move to the next buffer when there is only 1 window.")
	  (defvar pertab-previous-buffer-function 'previous-buffer "The function used to move to the previous buffer when there is only 1 window.")
	  
	  (defsubst pertab--get-tab-data (data)
	    "Get DATA from current tab. Borrowed from bufler."
	    (alist-get data (cdr (tab-bar--current-tab-find))))
	  
	  (defsubst pertab--get-tab-local-variables ()
	    "Gets this tab's alist of local variables."
	    (pertab--get-tab-data 'pertab-local-variables))
	  
	  (defsubst pertab--get-layout-symbol ()
	    "Gets this tab's layout symbol."
	    (pertab--get-tab-data 'pertab-layout))
	  
	  (defun pertab--set-tab-parameter (parameter tab value)
	    "Set PARAMETER in TAB to VALUE and return it. Stolen directly from Bufler."
	    (setf (alist-get parameter (cdr tab)) value))
	  
	  (defun pertab--add-tab-local-variables ()
	    "If the current tab has no tab-local-variables, set them to their default values."
	    (unless (pertab--get-tab-local-variables)
	      (pertab--set-tab-parameter 'pertab-local-variables (tab-bar--current-tab-find) (copy-alist pertab--default-tab-local-variables))))
	  
	  (defun pertab-set-tab-local (name value)
	    "Set the tab local variable named NAME to VALUE."
	    (pertab--add-tab-local-variables)
	    (setf (alist-get name (pertab--get-tab-local-variables)) value))
	  
	  (defun pertab--restore-tab-local-variables ()
	    "Restores all tab local variables."
	    (pertab--add-tab-local-variables)
	    (dolist (pair (pertab--get-tab-local-variables))
	      (set (car pair) (cdr pair))))
	  
	  (defun pertab--get-current-layout ()
	    "Returns the current tab's layout object."
	    (unless (pertab--get-layout-symbol)
	      (pertab--set-tab-parameter 'pertab-layout (tab-bar--current-tab-find) pertab-default-layout))
	    (alist-get (pertab--get-layout-symbol) pertab--layout-registry))
	  
	  (defclass pertab-layout-manager ()
	    ((enter-fun :initarg :enter-fun
	  	      :type function
	  	      :custom function
	  	      :initform (lambda ())
	  	      :documentation "The function to run when entering the layout.")
	     (exit-fun :initarg :exit-fun
	  	     :type function
	  	     :custom function
	  	     :initform (lambda ())
	  	     :documentation "The function to run when exiting the layout.")
	     (horiz-split-fun :initarg :horiz-split-fun
	  		    :type function
	  		    :custom function
	  		    :initform (lambda ())
	  		    :documentation "The function to run when making a horizontal split.")
	     (vert-split-fun :initarg :vert-split-fun
	  		   :type function
	  		   :custom function
	  		   :initform (lambda ())
	  		   :documentation "The function to run when making a vertical split.")
	     (focus-left-fun :initarg :focus-left-fun
	  		   :type function
	  		   :custom function
	  		   :initform (lambda ())
	  		   :documentation "The function to run when focusing the window to the left.")
	     (focus-right-fun :initarg :focus-right-fun
	  		    :type function
	  		    :custom function
	  		    :initform (lambda ())
	  		    :documentation "The function to run when focusing the window to the right.")
	     (focus-up-fun :initarg :focus-up-fun
	  		 :type function
	  		 :custom function
	  		 :initform (lambda ())
	  		 :documentation "The function to run when focusing the window above.")
	     (focus-down-fun :initarg :focus-down-fun
	  		   :type function
	  		   :custom function
	  		   :initform (lambda ())
	  		   :documentation "The function to run when focusing the window below.")
	     (move-left-fun :initarg :move-left-fun
	  		  :type function
	  		  :custom function
	  		  :initform (lambda ())
	  		  :documentation "The function to run when moving the window left.")
	     (move-right-fun :initarg :move-right-fun
	  		   :type function
	  		   :custom function
	  		   :initform (lambda ())
	  		   :documentation "The function to run when moving the window right.")
	     (move-up-fun :initarg :move-up-fun
	  		:type function
	  		:custom function
	  		:initform (lambda ())
	  		:documentation "The function to run when moving the window upwards.")
	     (move-down-fun :initarg :move-down-fun
	  		  :type function
	  		  :custom function
	  		  :initform (lambda ())
	  		  :documentation "The function to run when moving the window downwards.")
	     (close-window-fun :initarg :close-window-fun
	  		     :type function
	  		     :custom function
	  		     :initform (lambda ())
	  		     :documentation "The function to run when closing the current window.")
	     (lighter :initarg :lighter
	  		     :type string
	  		     :custom string
	  		     :initform "   "
	  		     :documentation "The 3 character string to represent this layout with.")))
	  
	  (defun pertab-register-layout (key local-variables manager)
	    "Register a new layout to the layout registry under KEY. LOCAL-VARIABLES should be an alist with variable names as the keys & their default values as the values. MANAGER is the 'pertab-layout-manager' that you wish to register."
	    (setf (alist-get key pertab--layout-registry) manager)
	    (dolist (pair local-variables)
	      (setf (alist-get (car pair) pertab--default-tab-local-variables) (cdr pair))))
	  
	  (defun pertab-get-lighter ()
	    "Return the current layout's lighter."
	    (oref (pertab--get-current-layout) lighter))
	  
	  (defun pertab--do-window-management-action (action)
	    "Run the function in the current layout stored in the field of 'pertab-layout-manager' named ACTION."
	    (funcall (eieio-oref (pertab--get-current-layout) action)))
	  
	  (defun pertab-horizontal-split ()
	    "Split the current window horizontally, according to the current layout's rules."
	    (interactive)
	    (pertab--do-window-management-action 'horiz-split-fun))
	  
	  (defun pertab-vertical-split ()
	    "Split the current window vertically, according to the current layout's rules."
	    (interactive)
	    (pertab--do-window-management-action 'vert-split-fun))
	  
	  (defun pertab--focus-action (command mono)
	    "Run the window management action COMMAND. If only 1 window is active, run MONO instead."
	    (if (eq (count-windows) 1)
	        (funcall mono))
	    (pertab--do-window-management-action command))
	  
	  (defun pertab-focus-left ()
	    "Put keyboard focus on the window to the left, according to the current layout's rules."
	    (interactive)
	    (pertab--focus-action 'focus-left-fun pertab-previous-buffer-function))
	  
	  (defun pertab-focus-right ()
	    "Put keyboard focus on the window to the right, according to the current layout's rules. If there is only one window, go to the next buffer instead."
	    (interactive)
	    (pertab--focus-action 'focus-right-fun pertab-next-buffer-function))
	  
	  (defun pertab-focus-up ()
	    "Put keyboard focus on the window above, according to the current layout's rules. If there is only one window, go to the next buffer instead."
	    (interactive)
	    (pertab--focus-action 'focus-up-fun pertab-previous-buffer-function))
	  
	  (defun pertab-focus-down ()
	    "Put keyboard focus on the window below, according to the current layout's rules. If there is only one window, go to the next buffer instead."
	    (interactive)
	    (pertab--focus-action 'focus-down-fun pertab-next-buffer-function))
	  
	  (defun pertab-move-left ()
	    "Move the current window to the left, according to the current layout's rules. If there is only one window, go to the next buffer instead."
	    (interactive)
	    (pertab--do-window-management-action 'move-left-fun))
	  
	  (defun pertab-move-right ()
	    "Move the current window to the right, according to the current layout's rules. If there is only one window, go to the next buffer instead."
	    (interactive)
	    (pertab--do-window-management-action 'move-right-fun))
	  
	  (defun pertab-move-up ()
	    "Move the current window upwards, according to the current layout's rules."
	    (interactive)
	    (pertab--do-window-management-action 'move-up-fun))
	  
	  (defun pertab-move-down ()
	    "Move the current window downwards, according to the current layout's rules."
	    (interactive)
	    (pertab--do-window-management-action 'move-down-fun))
	  
	  (defun pertab-close-window ()
	    "Close the current window, according to the current layout's rules."
	    (interactive)
	    (if (eq (count-windows) 1)
	        (tab-close)
	      (pertab--do-window-management-action 'close-window-fun)))
	  
	  (defun pertab-kill-buffer-close-window ()
	    "Kill the current buffer & close the current window, according to the current layout's rules."
	    (interactive)
	    (kill-current-buffer)
	    (pertab-close-window))
	  
	  (defun pertab--enter-layout ()
	    "Run the current layout's entry function."
	    (pertab--do-window-management-action 'enter-fun))
	  
	  (defun pertab--exit-layout (&rest args)
	    "Run the current layout's exit function. Ignores ARGS."
	    (pertab--do-window-management-action 'exit-fun))
	  
	  (defun pertab--after-switch-to-tab (&rest args)
	    "Meant to be run in 'tab-bar-select-tab' & 'tab-bar-new-tab-to's hooks. Ignores ARGS."
	    (pertab--restore-tab-local-variables)
	    (pertab--enter-layout))
	  
	  (define-minor-mode pertab-mode
	    "Adds per-tab layout management, similar to dwm's pertag patch."
	    :global t
	    (if pertab-mode
	        (progn
	  	(advice-add 'tab-bar-select-tab :before 'pertab--exit-layout)
	  	(advice-add 'tab-bar-new-tab-to :before 'pertab--exit-layout)
	  	(add-hook 'tab-bar-tab-post-select-functions 'pertab--after-switch-to-tab)
	  	(add-hook 'tab-bar-tab-post-open-functions 'pertab--after-switch-to-tab))
	      (advice-remove 'tab-bar-select-tab 'pertab--exit-layout)
	      (advice-remove 'tab-bar-new-tab-to 'pertab--exit-layout)
	      (remove-hook 'tab-bar-tab-post-select-functions 'pertab--after-switch-to-tab)
	      (remove-hook 'tab-bar-tab-post-open-functions 'pertab--after-switch-to-tab)))
	  
	  (defun pertab--set-layout (layout)
	    "Set the current tab's layout to LAYOUT."
	    (pertab--exit-layout)
	    (pertab--set-tab-parameter 'pertab-layout (tab-bar--current-tab-find) layout)
	    (pertab--enter-layout))
	  
	  (defun pertab-layout-menu ()
	    "Select a new layout with 'completing-read'."
	    (interactive)
	    (pertab--set-layout (intern (completing-read "Selecta a layout: " pertab--layout-registry))))
	  
	  ;; From dmacs
	  (defvar pertab-monocle--old-window-state nil "Window state prior to entering monocole layout.")
	  (defvar pertab-monocle-enter-hook nil "Hook run when entering monocole layout.")
	  (defvar pertab-monocle-exit-hook nil "Hook run when exiting monocole layout.")
	  
	  (defun pertab-monocle-enter ()
	    "Sets up monocole layout."
	    (setq pertab-monocle--old-window-state (current-window-configuration))
	    (delete-other-windows)
	    (run-hooks 'pertab-monocle-enter-hook))
	  
	  (defun pertab-monocle-exit ()
	    "Tears down monocle layout."
	    (set-window-configuration pertab-monocle--old-window-state)
	    (run-hooks 'pertab-monocle-exit-hook))
	  
	  (pertab-register-layout 'monocle '() (pertab-layout-manager :lighter "[M]"
	  							    :enter-fun 'pertab-monocle-enter
	  							    :exit-fun 'pertab-monocle-exit))
	  
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
	  
	  (pertab-mode)
	  (setq pertab-default-layout 'master-stack
	        pertab-next-buffer-function 'bufler-cycle-buffers-forward
	        pertab-previous-buffer-function 'bufler-cycle-buffers-backward)
	  (add-hook 'pertab-follow-enter-hook (lambda () (golden-ratio-mode -1)))
	  (add-hook 'pertab-follow-exit-hook (lambda () (golden-ratio-mode +1)))
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
