{
  flake.homeModules.emacs = { pkgs, ... }: {
    programs.emacs.init = {
      keybinds.evil.keys = {
	forward = "i";
	backward = "n";
	up = "o";
	down = "e";
	prefer-visual-line = true;
	evil-collection-swap-keys = ''
          "x" "B"
          "X" "b"
          "u" "W"
          "U" "w"
          "j" "u"
          "a" ":"
          "m" "n"
          "M" "N"
          "h" "m"
          "b" "g"
	'';
      };
      usePackage.evil = {
	generalOneConfig = {
	  ":n" = {
	    "I" = "'evil-window-top";
	    "C-i" = "'evil-goto-line";
	    "N" = "'evil-window-bottom";
	    "C-n" = "'evil-goto-first-line";
	    "E" = "'evil-scroll-down";
	    "O" = "'evil-scroll-up";
	    "x" = "'evil-backward-WORD-begin";
	    "X" = "'evil-backward-word-begin";
	    "C-x" = "'evil-backward-WORD-end";
	    "j" = "'evil-undo";
	    "J" = "'evil-redo";
	    "a" = "'evil-ex";
	    "r" = "'evil-insert-line";
	    "R" = "'evil-open-above";
	    "s" = "'evil-append-line";
	    "S" = "'evil-open-below";
	    "t" = "'evil-insert";
	    "T" = "'evil-append";
	    "C-t" = "'evil-replace-state";
	    "u" = "'evil-forward-WORD-begin";
	    "U" = "'evil-forward-word-begin";
	    "C-u" = "'evil-forward-WORD-end";
	    "-" = "'evil-jump-backward";
	    "_" = "'evil-jump-forward";
	    "m" = "'evil-search-next";
	    "M" = "'evil-search-previous";
	    "k" = "'evil-delete-char";
	    "K" = "'evil-substitute";
	    "c" = "'evil-visual-char";
	    "C" = "'evil-visual-line";
	    "C-c" = "'evil-visual-block";
	    "v" = "'evil-delete";
	    "V" = "'evil-change";
	    "C-v" = "'evil-delete-line";
	    "d" = "'evil-yank";
	    "D" = "'evil-yank-line";
	    "G" = "'evil-paste-after";
	    ";" = "'evil-end-of-visual-line";
	    ":" = "'evil-end-of-line";
	    "C-;" = "'evil-end-of-line";
	    "p" = "'ergo-word-delete";
	    "P" = "'ergo-word-change";
	    "C-p" = "'ergo-word-change";
	    "$" = "'evil-execute-macro";
	    "~" = "'evil-record-macro";
	    "C-z" = "'evil-goto-last-change-reverse";
	    "w" = "'evil-repeat";
	    "W" = "'evil-ex-repeat";
	    "C-w" = "'evil-ex-repeat";
	    "l" = "'evil-shift-right-line";
	    "L" = "'evil-shift-left-line";
	    "C-l" = "'evil-shift-left-line";
	    "/" = "'isearch-forward-regexp";
	    "?" = "'isearch-backward-regexp";
	    "y" = "'evil-shift-right";
	    "Y" = "'evil-shift-left";
	    "C-s" = "'evil-write";
	  } ;
	  ":m" = {
	    "I" = "'evil-window-top";
	    "C-i" = "'evil-goto-line";
	    "N" = "'evil-window-bottom";
	    "C-n" = "'evil-goto-first-line";
	    "C-e" = "'evil-scroll-page-down";
	    "C-o" = "'evil-scroll-page-up";
	    "a" = "'evil-ex";
	    "h" = "'evil-set-marker";
	    "m" = "'evil-search-next";
	    "M" = "'evil-search-previous";
	    "-" = "'evil-jump-backward";
	    "_" = "'evil-jump-forward";
	    "/" = "'isearch-forward-regexp";
	    "?" = "'isearch-backward-regexp";
	    "f" = "'evil-first-non-blank-of-visual-line";
	    "F" = "'evil-beginning-of-visual-line";
	    "C-f" = "'evil-first-non-blank";
	    "B" = "'evil-goto-line";
	    "C-M-o" = "'scroll-other-window-down";
	    "C-M-e" = "'scroll-other-window";
	  };
	  ":v" = {
	    "U" = "'evil-forward-word-begin";
	    "u" = "'evil-forward-WORD-begin";
	    "X" = "'evil-backward-word-begin";
	    "x" = "'evil-backward-WORD-begin";
	    "v" = "'evil-delete-char";
	    "V" = "'evil-substitute";
	    "C-v" = "'evil-substitute";
	    "t" = "evil-outer-text-objects-map";
	    "s" = "evil-inner-text-objects-map";
	    "l" = "'evil-invert-case";
	    "y" = "'evil-shift-right";
	    "Y" = "'evil-shift-left";
	    "C-t" = "'evil-replace";
	    "R" = "'evil-insert-line";
	    "C-r" = "'evil-append-line";
	    "d" = "'evil-yank-line";
	    "D" = "'evil-yank-line";
	    "C-d" = "'evil-yank-line";
	    "/" = "'isearch-forward-regexp";
	    "?" = "'isearch-backward-regexp";
	    "G" = "'evil-paste";
	  };
	  ":i" = {
	    "C-s" = "'insert-char";
	    "C-k" = "'kill-line";
	  };
	};
	extraConfig = ''
	  :general-config
	  (general-swap-key nil '(motion normal visual)
	    "g" "b"
	    "z" "q"
	    "Z" "Q")
	  
	  (:keymaps 'override
	  	  :states '(normal visual)
	  	  "g" 'evil-paste-before
	  	  "z" 'evil-jump-item
	  	  "Z" 'evil-goto-last-change)
	  
	  (:keymaps 'override
	  	  :states '(operator visual)
	  	  "i" 'evil-forward-char
	  	  "s" evil-inner-text-objects-map
	  	  "t" evil-outer-text-objects-map)
	  
	  ('normal "bl" 'consult-goto-line
	  	 "b/" 'consult-keep-lines)
	'';
	config = ''
	  (evil-define-operator ergo-word-delete (beg end type register yank-handler)
	    "Delete word."
	    :motion evil-a-word
	    (evil-delete beg end type register yank-handler))
	  
	  (evil-define-operator ergo-word-change (beg end type register yank-handler)
	    "Delete word."
	    :motion evil-inner-word
	    (evil-change beg end type register yank-handler))
	'';
      };
    };
  };
}
