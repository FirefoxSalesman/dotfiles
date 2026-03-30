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
	  '';
	};
      };
    };
  };
}
