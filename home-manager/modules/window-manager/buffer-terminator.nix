{ ... }:

{
  flake.homeModules.exwm = { ... }:
  {
    programs.emacs.init.usePackage.buffer-terminator = {
      enable = true;
      preface = ''
	(defun efs/check-for-roll-buffers ()
	  "Check for roll buffers for buffer-terminator."
	  (if (-contains? (mapcar 'roll-pane-buffer
				  (car (-filter (lambda (x) (not (eq nil x)))
						(mapcar (lambda (tab)
							  (alist-get 'roll--panes (alist-get 'pertab-local-variables (cdr tab))))
							(funcall tab-bar-tabs-function)))))
			  (current-buffer))
	      ':keep
	    nil))
      '';
      ghookf = ["('on-first-buffer 'buffer-terminator-mode)"];
      setopt.buffer-terminator-rules-alist = [
	'''(kill-buffer-name-regexp . "magit-process:.\\*$")''
	'''(kill-buffer-name-regexp . "magit-diff:.\\*$")''
	'''(keep-buffer-name . ("tasks.org"))''
	"'(keep-buffer-property . special)"
	"'(keep-buffer-property . process)"
	"'(keep-buffer-property . visible)"
	"'(call-function . efs/check-for-roll-buffers)"
	"'(kill-buffer-property . inactive)"
	"'(return . :keep)"
      ];
    };
  };
}
