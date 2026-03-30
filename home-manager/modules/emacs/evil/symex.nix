{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      ide.symex = true;  
      usePackage.symex = {
	enable = true;
	defer = true;
	generalTwo.":n"."(racket-repl-mode-map lisp-interaction-mode-map lisp-mode-map)"."RET" = "'symex-mode-interface";
	config = ''
	  (symex-mode)
	  (repeaters-define-maps
	   '(("symex-visual-line"
	      symex-next-visual-line "e"
	      symex-previous-visual-line "o")))
	  
	  (require 'evil-easymotion)
	  
	  (evilem-make-motion efs/evilem-motion-symex-go-forward #'symex-go-forward)
	  (evilem-make-motion efs/evilem-motion-symex-go-backward #'symex-go-backward)
	  (evilem-make-motion efs/evilem-motion-symex-go-down #'symex-go-down)
	  (evilem-make-motion efs/evilem-motion-symex-go-up #'symex-go-up)    
	'';
	generalOneConfig.evil-symex-state-map = {
	  "n" = "'symex-go-backward";
	  "H-n" = "'efs/evilem-motion-symex-go-backward";
	  "H-o" = "'efs/evilem-motion-symex-go-down";
	  "C-o" = "'symex-climb-branch";
	  "o" = "'symex-go-down";
	  "H-e" = "'efs/evilem-motion-symex-go-up";
	  "C-e" = "'symex-descend-branch";
	  "e" = "'symex-go-up";
	  "i" = "'symex-go-forward";
	  "H-i" = "'efs/evilem-motion-symex-go-forward";
	  "bn" = "'evil-backward-char";
	  "bi" = "'evil-forward-char";
	  "d" = "'symex-yank";
	  "D" = "'symex-yank-remaining";
	  "G" = "'symex-paste-after";
	  "g" = "'symex-paste-before";
	  "k" = "'symex-delete";
	  "C-k" = "'symex-delete-backward";
	  "p" = "'symex-delete-remaining";
	  "K" = "'symex-change";
	  "P" = "'symex-change-remaining";
	  "N" = "'symex-shift-backward";
	  "I" = "'symex-shift-forward";
	  "M-N" = "'symex-shift-backward-most";
	  "M-I" = "'symex-shift-forward-most";
	  "M-n" = "'symex-goto-first";
	  "M-i" = "'symex-goto-last";
	  "t" = "'symex-insert-at-beginning";
	  "T" = "'symex-append-at-end";
	  "S" = "'symex-open-line-after";
	  "R" = "'symex-open-line-before";
	  "j" = "'evil-undo";
	  "J" = "'evil-redo";
	  "s" = "'symex-append-after";
	  "r" = "'symex-insert-before";
	  "w" = "'evil-repeat";
	  "C-w" = "'evil-repeat-pop";
	  "W" = "'evil-ex-repeat";
	  "a" = "'evil-ex";
	  "~" = "'evil-record-macro";
	  "$" = "'evil-execute-macro";
	};
      };
    };
  };
}
