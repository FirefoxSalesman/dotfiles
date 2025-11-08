{ lib, pkgs, inputs, config, ... }:

{
  programs.emacs.init = {
    keybinds = {
      evil = {
        enable = true;
        surround = true;
        keys = {
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
      };
      leader-key = {
        enable = true;
        globalPrefix = "s";
      };
      doomEscape.enable = true;
      undo.enable = true;
      whichKey = {
        enable = true;
        posframe = {
          enable = true;
          unparent = true;
        };
      };
      god.enable = true;
      electricPair.enable = true;
      evilNerdCommenter.enable = true;
      avy = {
        enable = true;
        evilModifierKey = "H";
      };
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
          
          (evil-define-operator ergo-word-delete (beg end type register yank-handler)
            "Delete word."
            :motion evil-a-word
            (evil-delete beg end type register yank-handler))
          
          (evil-define-operator ergo-word-change (beg end type register yank-handler)
            "Delete word."
            :motion evil-inner-word
            (evil-change beg end type register yank-handler))
          
          (repeaters-define-maps
           '(("flyspell"
              evil-prev-flyspell-error "S"
              evil-next-flyspell-error "s")))
        '';
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
      };
      
      emacs.generalOneConfig = {
        help-map."A" = ''`("Arch Wiki" . ,(cmd! (async-shell-command "${pkgs.wiki}/bin/wiki")))'';
        global-leader = {
          "l" = ''`("Compile" . ,(cmd! (if (project-current) (project-compile) (compile (read-string "Compile command: " "make -k")))))'';
          "L" = ''`("Recompile" . ,(cmd! (if (project-current) (project-recompile) (recompile))))'';
          "u" = ''`("Mount USB" . ,(cmd! (start-process-shell-command "udisksmenu" nil "${pkgs.udisksmenu}/bin/udisksmenu")))'';
        };
      };
    
      evil-collection.setopt.evil-collection-unimpaired-want-repeat-mode-integration = true;
      
      evil-surround.generalTwoConfig = {
        ":v".evil-surround-mode-map = {
          "r" = "'evil-surround-region";
          "R" = "'evil-surround-region";
        };
        ":o".evil-surround-mode-map = {
          "s" = "nil";
          "r" = "'evil-surround-edit";
          "R" = "'evil-Surround-edit";
        };
      };
      
      evil-easymotion = {
        generalOne.":nvo" = {
          "H-m" = "'evilem-motion-search-next";
          "H-U" = "'evilem-motion-forward-word-begin";
          "H-u" = "'evilem-motion-forward-WORD-begin";
          "H-X" = "'evilem-motion-backward-word-begin";
          "H-x" = "'evilem-motion-backward-WORD-begin";
          "H-M" = "'evilem-motion-search-previous";
          "H-)" = "'evilem-motion-forward-sentence-begin";
          "H-(" = "'evilem-motion-backward-sentence-begin";
        };
        custom.avy-dispatch-alist = [
          "'(?l . avy-action-ispell)"
          "'(?o . nix-emacs-avy-action-embark)"
          "'(?h . avy-action-helpful)"
          "'(?g . avy-action-yank)"
          "'(?p . avy-action-teleport)"
          "'(?q . nix-emacs-avy-action-fold)"
        ];
        setopt = {
          avy-keys = "'(?c ?r ?s ?t ?b ?f ?n ?e ?i ?a)";
          avy-all-windows = false;
        };
        config = ''
          (defun avy-action-helpful (pt)
            "Get documentation for thing at point."
            (nix-emacs-base-avy-action 'helpful-at-point pt))
          
          (defun avy-action-repeat (pt)
            "Repeat the last evil command."
            (nix-emacs-base-avy-action (lambda () (evil-repeat 1)) pt))
        '';
      };
      
      evil-mc = {
        enable = true;
        defer = true;
        command = ["evil-mc-pause-cursors" "evil-mc-make-cursor-here"];
        gfhookf = [''('doom-escape (lambda () (when (and (featurep 'evil-mc) (evil-mc-has-cursors-p))
      						 (evil-mc-undo-all-cursors)
      						 (evil-mc-resume-cursors) t)))''];
        generalOne = {
          ":nv"."bz" = "'evil-mc-hydra/body";
          global-leader."C" = "'evil-mc-hydra/body";
        };
        config = ''
          (global-evil-mc-mode)
          
          ;; Don't mess with my macros.
          ;; https://github.com/gabesoft/evil-mc/issues/83
          (defun ~+multiple-cursors-evil-mc-write-cursor-state-a (state)
            "Write the state of the real cursor with values from STATE."
            (let ((names (evil-mc-get-cursor-variables)))
              (dolist (name names)
                (when (boundp name)
                  (let ((p (evil-mc-get-cursor-property state name)))
                    (when (not
                           (or
                            (eq name 'register-alist)
                            (eq name 'evil-markers-alist)))
                      (set name p)))))))
          (advice-add #'evil-mc-write-cursor-state :override #'~+multiple-cursors-evil-mc-write-cursor-state-a)
        '';
        extraConfig = ''
          :pretty-hydra
          ((:color pink :pre (evil-mc-pause-cursors))
           ("Search"
            (("m" #'evil-mc-make-and-goto-next-match "Search forward")
             ("M" #'evil-mc-make-and-goto-prev-match "Search backward")
             ("C-m" #'evil-mc-skip-and-goto-next-match "Skip forward")
             ("C-M" #'evil-mc-skip-and-goto-prev-match "Skip backward"))
            "Undo"
            (("q" #'evil-mc-undo-all-cursors)
             ("j" #'evil-mc-undo-last-added-cursor))
            "Pause/Resume"
            (("r" #'evil-mc-resume-cursors "Resume")
             ("p" #'evil-mc-pause-cursors "Pause")
             ("<return>" #'evil-mc-resume-cursors "Quit" :color blue))
            "Create Cursors"
            (("h" #'evil-mc-make-all-cursors "All")
             ("s" #'evil-mc-make-cursor-here "Here")
             ("E" #'evil-mc-make-cursor-move-next-line "Next Line")
             ("O" #'evil-mc-make-cursor-move-prev-line "Prev Line"))))
        '';
      };
      
      evil-owl = {
        enable = true;
        setopt = {
          evil-owl-max-string-length = 50;
          evil-owl-extra-posframe-args = [ "':width" 50 "':height" 20 ];
          evil-owl-display-method = "'posframe";
        };
        ghookf = ["('evil-mode 'evil-owl-mode)"];
      };
      
      evil-exchange = {
        enable = true;
        generalOne = {
          evil-operator-state-map."k" = "'evil-exchange/cx";
          evil-visual-state-map."k" = "'evil-exchange";
        };
        gfhookf = ["('doom-escape (lambda () (when (featurep 'evil-exchange) (evil-exchange-cancel))))"];
      };
    
      symex = {
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

      evil-org.config = ''
        (evil-define-key 'operator 'evil-org-mode
          "i" 'evil-forward-char)
        (evil-define-key 'normal 'evil-org-mode
          "o" 'evil-previous-visual-line
          "O" 'evil-scroll-up
          "R" 'evil-org-open-above
          "S" 'evil-org-open-below
          "x" 'evil-backward-WORD-begin
          "X" 'evil-backward-word-begin
          "d" 'evil-yank)
        (evil-define-key 'symex 'evil-org-mode
          "R" 'evil-org-open-above
          "S" 'evil-org-open-below)
        (evil-define-key 'visual 'evil-org-mode
          "i" 'evil-forward-char
          "s" evil-inner-text-objects-map)
      '';
      
      evil-org-agenda = {
        generalTwoConfig.":m".evil-org-agenda-mode-map = {
          "bn" = "'org-agenda-next-item";
          "bI" = "'evil-window-bottom";
          "I" = "'org-agenda-do-date-later";
          "C-S-i" = "'org-agenda-todo-nextset"; # Original binding "C-S-<right>"
          "l" = "'org-agenda-diary-entry";
        };
      };
      
      ewal-evil-cursors = {
        enable = true;
        demand = true;
        config = ''(ewal-evil-cursors-get-colors :apply t)'';
      };
    };
  };
}
