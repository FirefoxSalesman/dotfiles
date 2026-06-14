{ inputs, ... }:

{
  flake.homeModules.emacs = { lib, pkgs, config, ... }:

  {
    programs.emacs.init = {
      keybinds = {
	evil.enable = true;
	evilNerdCommenter.enable = true;
	god.enable = true;
      };
      usePackage = {
	evil = {
	  gfhookf = ["('doom-escape 'evil-normal-state)"];
	  config = ''
	    (evil-ex-define-cmd "q" `,(cmd! (prescient--save) (save-buffers-kill-emacs)))
	    (evil-ex-define-cmd "Undotree" 'vundo)
	    (evil-ex-define-cmd "k[ill]" 'kill-current-buffer)
	    
	    (evil-set-initial-state 'dashboard-mode 'normal)
	    
	    (dolist (command '(consult-grep
	    		   consult-line
	    		   isearch-forward-regexp
	    		   evilem-motion-previous-visual-line
	    		   evilem-motion-next-line
	    		   evilem-motion-forward-WORD-begin
	    		   evilem-motion-backward-WORD-begin
	    		   evilem-motion-search-next
	    		   evilem-motion-search-previous
	    		   find-file
	    		   consult-fd
	    		   nix-emacs/consult-header))
	      (evil-add-command-properties command :jump t))
	    
	    (repeaters-define-maps
	     '(("flyspell"
	        evil-prev-flyspell-error "S"
	        evil-next-flyspell-error "s")))
	    
	    (defvar stack-the-states-current-stack 'evil)
	    (defvar stack-the-states-out-of-bounds-hook '())
	    (defvar stack-the-states-stacks '())
	    
	    (defclass stack-the-states-stack-manager ()
	      (default-state :initarg :default-state
	    		 :type function
	    		 :custom function
	    		 :documentation "The function to switch to the default evil state.")
	      (state-stack :initarg :state-stack
	    	       :documentation "A list of evil state switching functions"))
	    
	    (defun stack-the-states--get-current-state ()
	      "Return the current stack of evil states."
	      (alist-get stack-the-states-current-stack stack-the-states-stacks))
	    
	    (defun stack-the-states--default ()
	      "Return to the current stack's default evil state."
	      (funcall (eieio-oref (stack-the-states--get-current-state) 'default-state)))
	    
	    (defun stack-the-states--get-state-stack ()
	      "Return to the current stack's default evil state."
	      (eieio-oref (stack-the-states--get-current-state) 'state-stack))
	    
	    (defun stack-the-states--do-action (action)
	      "Execute ACTION on the position of the current state in the stack, or return to the default state if we are outside of the stack."
	      (let ((current-state (intern (concat "evil-" (symbol-name evil-state) "-state")))
	          (state-stack (stack-the-states--get-state-stack)))
	      (if (not (-contains? state-stack current-state))
	          (stack-the-states--default)
	        (funcall action (seq-position state-stack current-state)))))
	    
	    (defun stack-the-states-go-up ()
	      "Go up 1 state, or return to the default state if we are outside the stack."
	      (interactive)
	      (stack-the-states--do-action (lambda (pos)
	    				 (let (newpos (+ 1 pos))
	    				   (if (length< (stack-the-states--get-state-stack) newpos))))))
	  '';
	};
      };
    };
  };
}
