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
	doom-escape.ghookf = lib.mkForce ["('stack-the-states-out-of-bounds 'doom/escape)"];
	evil = {
	  gfhookf = ["('doom-escape 'stack-the-states--default)"];
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
	      ((default-state :initarg :default-state
	    		 :type function
	    		 :custom function
	    		 :documentation "The function to run when entering the layout.")
	      (state-stack :initarg :state-stack
	    	       :documentation "A list of evil state switching functions")))
	    
	    (defun stack-the-states--get-current-state ()
	      "Return the current stack of evil states."
	      (alist-get stack-the-states-current-stack stack-the-states-stacks))
	    
	    (defun stack-the-states--default ()
	      "Return to the current stack's default evil state."
	      (funcall (eieio-oref (stack-the-states--get-current-state) 'default-state)))
	    
	    (defun stack-the-states--get-state-stack ()
	      "Return to the current stack's default evil state."
	      (eieio-oref (stack-the-states--get-current-state) 'state-stack))
	    
	    (defun stack-the-states--do-action (inc)
	      "Switch to the next/previous state in the stack, or return to the default state if we are outside of the stack. INC is the function that increments our position in the let."
	      (let ((current-state (intern (concat "evil-" (symbol-name evil-state) "-state")))
	          (state-stack (stack-the-states--get-state-stack)))
	      (if (not (-contains? state-stack current-state))
	          (stack-the-states--default)
	        (let* ((pos (seq-position state-stack current-state))
	    	   (newpos (funcall inc pos 1)))
	          (if (or (>= newpos (length state-stack)) (< newpos 0))
	    	  (run-hooks 'stack-the-states-out-of-bounds-hook)
	    	(funcall (nth newpos state-stack)))))))
	    
	    (defun stack-the-states-go-up ()
	      "Go up 1 state, or return to the default state if we are outside the stack."
	      (interactive)
	      (stack-the-states--do-action '+))
	    
	    (defun stack-the-states-go-down ()
	      "Go down 1 state, or return to the default state if we are outside the stack."
	      (interactive)
	      (stack-the-states--do-action '-))
	    
	    (defun stack-the-states-make-stack (name manager)
	      "Add a stack manager to 'stack-the-states-stacks'. NAME is a symbol containing the stack's name. MANAGER is a 'stack-the-states-stack-manager'."
	      (add-to-list 'stack-the-states-stacks `(,name . ,manager)))
	    
	    (defun stack-the-states-switch-stack ()
	      "Switch to a different stack of states."
	      (interactive)
	      (setq-local stack-the-states-current-stack (intern (completing-read "Select a stack: " stack-the-states-stacks)))
	      (stack-the-states--default))
	    
	    (stack-the-states-make-stack 'evil (stack-the-states-stack-manager :default-state 'evil-normal-state :state-stack '(evil-insert-state evil-normal-state)))
	    (stack-the-states-make-stack 'symex (stack-the-states-stack-manager :default-state 'evil-normal-state :state-stack '(evil-insert-state evil-symex-state evil-normal-state)))
	    (stack-the-states-make-stack 'motion (stack-the-states-stack-manager :default-state 'evil-motion-state :state-stack '(evil-motion-state)))
	    (stack-the-states-make-stack 'emacs (stack-the-states-stack-manager :default-state 'evil-emacs-state :state-stack '(evil-emacs-state evil-god-state)))
	    
	    (general-def "C-<escape>" 'stack-the-states-switch-stack)
	    (general-def '(normal insert emacs god symex motion visual operator) "<escape>" 'stack-the-states-go-up)
	    (general-def '(normal god symex) "M-t" 'stack-the-states-go-down)
	    
	    (hook! 'special-mode (local! stack-the-states-current-stack 'motion))
	    (hook! (gen-mode-hooks '(bash-ts clojure c-ts lisp csharp-ts css-ts elisp erlang-ts fennel gdscript-ts go-ts haskell-ts html-ts hy java-ts js-ts json-ts json5-ts julia-ts kotlin-ts lua-ts org make nix-ts purescript python-ts racket ess-r ruby-ts rust-ts scala-ts scheme svelte-ts toml-ts typescript-ts yaml-ts zig)) (local! stack-the-states-current-stack 'symex))
	  '';
	};
      };
    };
  };
}
