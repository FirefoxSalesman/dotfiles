{
  flake.homeModules.exwm = { ... }:
  {
    programs.emacs.init.usePackage.ace-window = {
      enable = true;
      setopt = {
	aw-scope = "'frame";
	aw-keys = "'(?c ?r ?s ?t ?n ?e ?i ?a)";
      };
      preface = ''
	(defun efs-aw-split-window-elwm (window)
	  "Split WINDOW horizontally."
	  (select-window window)
	  (elwm-split-window))
	
	(defun efs-aw-kill-buffer-and-window (window)
	  "Delete window WINDOW & its associated buffer."
	  (aw-delete-window window t))
      '';
      init = ''(setopt aw-dispatch-alist '((?k aw-delete-window "Delete Window")
	(?K efs-aw-kill-buffer-and-window "Delete Buffer and Window")
                                           (?m aw-swap-window "Swap Windows")
                                           (?M aw-move-window "Move Window")
                                           (?d aw-copy-window "Copy Window")
                                           (?b aw-switch-buffer-in-window "Select Buffer")
                                           (?f aw-flip-window)
                                           (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
                                           (?x aw-execute-command-other-window "Execute Command Other Window")
                                           (?v efs-aw-split-window-elwm "Split Window")
                                           (?o delete-other-windows "Delete Other Windows")
                                           (?T aw-transpose-frame "Transpose Frame")
                                           (?? aw-show-dispatch-help)))'';
      general = {
	"s-H-o" = "'ace-window";
	"s-H-e" = "'ace-window";
      };
      config = ''
	(ace-window-posframe-mode)
	
	(defun aw--lead-overlay-posframe (path leaf)
	  (let* ((wnd (cdr leaf))
	         (str (format "%s" (apply #'string path)))
	         ;; It's important that buffer names are not unique across
	         ;; multiple invocations: posframe becomes very slow when
	         ;; creating new frames, and so being able to reuse old ones
	         ;; makes a huge difference. What defines "able to reuse" is
	         ;; something like: a frame exists which hasn't been deleted
	         ;; (with posframe-delete) and has the same configuration as
	         ;; the requested new frame.
	         (bufname (format " *aw-posframe-buffer-%s*" path)))
	    (with-selected-window wnd
	      (push bufname aw--posframe-frames)
	      (posframe-show bufname
	                     :string str
	                     :poshandler aw-posframe-position-handler
			     :refposhandler 'posframe-refposhandler-xwininfo
			     :parent-frame nil
	                     :font (face-font 'aw-leading-char-face)
	                     :foreground-color (face-foreground 'aw-leading-char-face nil t)
	                     :background-color (face-background 'aw-leading-char-face nil t)))))
	
	(general-add-advice 'ace-window :after (lambda (&rest args) (golden-ratio)))
	
	(defun aw--switch-buffer ()
	  "Call the buffer switching command appropriate to your setup."
	  (cond ((bound-and-true-p ivy-mode)
	         (ivy-switch-buffer))
	        ((bound-and-true-p ido-mode)
	         (ido-switch-buffer))
		((featurep 'consult)
		 (consult-buffer))
	        (t
	         (call-interactively 'switch-to-buffer))))
      '';
    };
  } ;
}
