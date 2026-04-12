{
  flake.homeModules.exwm = { pkgs, lib, ... }:

  {
  programs.emacs.init = {
    tools.exwm = {
      enable = true;
      wantMouseWarping = true;
      useGaps = true;
      bindings = {
	"?\\s-r" = "exwm-reset";
	"?\\s-a" = "evil-ex";
	"?\\s-d" = "app-launcher-run-app";
	"?\\s-t" = "proced";
	  
	# Movement
	"?\\s-e" = "elwm-next";
	"?\\s-o" = "elwm-prev";
	"?\\s-." = "other-frame";

	# Arrangement
	"?\\s-\\C-e" = "efs/nirify-forwards";
	"?\\s-\\C-o" = "efs/nirify-backwards";
	"?\\s-E" = "elwm-rotate-window";
	"?\\s-O" = "elwm-derotate-window";
	"?\\s-c" = "elwm-split-window";
	"?\\s-n" = "evil-window-move-far-left";
	"?\\s-i" = "evil-window-move-far-right";
	"?\\s-j" = "winner-undo";
	"?\\s-J" = "winner-redo";
	"?\\s-m" = "toggle-single-window";
	"?\\s-x" = "toggle-follow-mode";
	"?\\s-k" = "evil-window-delete";
	"?\\s-K" = "evil-delete-buffer-and-window";

	#Shell bindings
	"?\\s-s" = '',(cmd! (shell-command "slock"))'';
	"?\\s-y" = '',(cmd! (start-process-shell-command "maim" nil  "${lib.getExe pkgs.maim} ~/pic/screenshot.png"))'';
	"XF86MonBrightnessDown" = '',(cmd! (efs/alter-monitor-brightness 5 t))'';
	"XF86MonBrightnessUp" = '',(cmd! (efs/alter-monitor-brightness 5))'';
      };
      titleAlterations = {
	qutebrowser = ''(exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))'';
	mpv = ''(exwm-workspace-rename-buffer (format "Mpv: %s" exwm-title))'';
      };
    };
    usePackage.exwm = {
      gfhookf = [
	# When window "class" updates, use it to set the buffer name
	"('exwm-update-class (lambda () (exwm-workspace-rename-buffer exwm-class-name)))"
	# When EXWM starts up, do some extra configuration
	''('exwm-init (lambda ()
          (start-process-shell-command "xbanish" nil "${lib.getExe pkgs.xbanish}")))''
      ];
      # Ctrl+q will enable the next key to be sent directly
      generalOneConfig.exwm-mode-map = {
	"C-c" = "mode-specific-map";
      };
      custom = {
	exwm-manage-force-tiling = true;
	# Emacs everywhere
	exwm-input-simulation-keys = '''(([?\C-h] . [backspace]))'';
	exwm-workspace-number = 1;
	# This will need to be updated to the name of a display!  You can find
	# the names of your displays by looking at arandr or the output of xrandr
	exwm-randr-workspace-monitor-plist = '''(0 "eDP-1-1") (1 "HDMI-0")'';
	# Window focus should follow the mouse pointer
	mouse-autoselect-window = false;
	focus-follows-mouse = false;
	# These keys should always pass through to Emacs
	exwm-input-prefix-keys = lib.mkForce [
	  '''?\M-`''
	  '''?\C-^''
	  '''?\M-&''
	  "?\\s-\\M-'"
	  '''?\s-b''
	  '''?\M-u''
	  '''?\M-:''
	  '''?\s-o''
	  '''?\s-c''
	  '''?\s-v''
	  '''?\s-n''
	  '''?\s-i''
	  '''?\s-e''
	  '''?\s-f''
	  '''?\s-F''
	  '''?\s-u''
	  '''?\s-/''
	  '''?\s-\H-e''
	  '''?\s-\H-o''
	  "?\\s-'"	# popper-toggle-latest
	  '''?\s-\"'' # popper-toggle-type
	  '''?\s-\ ''
	  '''XF86AudioRaiseVolume''
	  '''XF86AudioLowerVolume''
	  '''XF86AudioMute''
	  '''?\M-\ ''
	];
      };
      init = ''
	;; From dmacs
	(defvar single-window--last-configuration nil "Last window configuration before calling `delete-other-windows'.")
	;; From dmacs
	(defun toggle-single-window ()
	  "Un-maximize current window.
	  If multiple windows are active, save window configuration and
	  delete other windows.  If only one window is active and a window
	  configuration was previously save, restore that configuration."
	  (interactive)
	  (if (= (count-windows) 1)
	      (when single-window--last-configuration
		(setq elwm-current-layout 'tile-vertical-left)
	        (set-window-configuration single-window--last-configuration))
	    (setq single-window--last-configuration (current-window-configuration))
	    (delete-other-windows)))
	
	(defun evil-delete-buffer-and-window ()
	  "kill the current buffer & its window"
	  (interactive)
	  (kill-current-buffer)
	  (evil-window-delete))
	
	(defun toggle-follow-mode ()
	  "If called while multiple windows are present, deactivates follow mode & kills all other windows.
	   If called on only 1 window, activates follow mode & splits the window."
	  (interactive)
	  (if (= (count-windows) 1)
	      (progn (follow-mode 1)
		     (split-window-right)
		     (setq elwm-current-layout 'follow))
	    (progn (follow-mode -1)
		   (delete-other-windows))))
	
	(defvar efs/monitor-brightness 27 "The percent brightness of the monitor.")
	
	(defun efs/alter-monitor-brightness (inc &optional neg)
	  "Alters the monitor's brightness.
	INC is the percent to increment the volume by.
	NEG subtracts if it is true."
	  (setopt efs/monitor-brightness (funcall (if neg '- '+) efs/monitor-brightness inc))
	  (shell-command (concat "brightnessctl -d intel_backlight set " (int-to-string efs/monitor-brightness) "%")))
	
	(defun efs/nirify-forwards ()
	  "Rotate the workspace, then rotate the buffers."
	  (interactive)
	  (elwm-rotate-window (prefix-numeric-value 1))
	  (with-selected-window (previous-window (frame-first-window))
	    (bufler-cycle-buffers-forward)))
	
	(defun efs/nirify-backwards ()
	  "Rotate the workspace, then rotate the buffers."
	  (interactive)
	  (elwm-derotate-window)
	  (with-selected-window (frame-first-window)
	    (bufler-cycle-buffers-forward)))
      '';
      config = ''
	(winner-mode)
	
	(repeaters-define-maps
	 '(("delete-windows"
	    evil-delete-buffer-and-window "K"
	    evil-window-delete "k")))
	(repeaters-define-maps
	 '(("input-keys"
	    exwm-input-send-next-key "q")))
	(exwm-input-set-key (kbd "s-<return>") 'efs/make-eshell)
      '';
      after = ["repeaters"];
    };
  };
  };
}
