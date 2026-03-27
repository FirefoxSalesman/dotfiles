{
  flake.homeModules.exwm = { ... }:

  {
    programs.emacs.init.usePackage.elwm = {
      enable = true;
      defer = true;
      command = ["elwm-next" "elwm-prev" "elwm-rotate-window" "elwm-derotate-window" "elwm-split-window"];
      config = ''
	(defun elwm-split-window ()
	  "Split window according to the current layout.
	  
	  Window in the master area can't be split, instead the last window
	  in the stack will be split.
	  
	  If selected window is window on the stack, the new window will be
	  created next to it, according to the current layout."
	  (interactive)
	  (let ((buf (current-buffer)))
	    (unless (or (eq elwm-current-layout 'monocle) (eq elwm-current-layout 'follow))
		(if (eq (count-windows) 1)
		    (if (eq elwm-current-layout 'tile-vertical-left)
			(evil-window-vsplit)
		      (evil-window-split))
		  (cond
		   ((eq elwm-current-layout 'tile-vertical-left)
		    (if (elwm--in-master-area-p)
			;; split the last window on the stack instead
			(set-window-buffer
			 (select-window (split-window (car (last (elwm--sorted-window-list))) nil nil))
			 buf)
		      (evil-window-split)))
		   ((eq elwm-current-layout 'tile-horizontal-top)
		    (if (elwm--in-master-area-p)
			;; split the last window on the stack instead
			(set-window-buffer
			 (select-window (split-window (car (last (elwm--sorted-window-list))) nil t))
			 buf)
		      (evil-window-vsplit))))))))
      '';
      preface = ''
	(defun elwm-next ()
	  "Go to the next visible window, or if there is 1 window, the next buffer in the workspace"
	  (interactive)
	  (if (eq (count-windows) 1)
	      (bufler-cycle-buffers-forward)
	    (elwm-activate-window)))
	(defun elwm-deactivate-window () (interactive) (elwm-activate-window (prefix-numeric-value -1)))
	(defun elwm-prev ()
	  "Go to the previous visible window, or if there is 1 window, the previous buffer in the workspace"
	  (interactive)
	  (if (eq (count-windows) 1)
	      (bufler-cycle-buffers-backward)
	    (elwm-deactivate-window)))
	(defun elwm-derotate-window () (interactive) (elwm-rotate-window (prefix-numeric-value -1)))
      '';
    };
  } ;
}
