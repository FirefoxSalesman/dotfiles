  { inputs, config, ... }:

  {
    programs.emacs.init = {
      keybinds.evil = {
        enable = true;
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
      usePackage = {
        evil = {
          gfhook = ["('doom-escape-hook 'evil-normal-state)"];
          generalOne = {
            "'insert" = {
              "C-s" = "'insert-char";
              "C-k" = "'kill-line";
            };
            "'normal"."C-s" = "'evil-write";
          };
          config = ''
            (evil-ex-define-cmd "q" '(lambda () (interactive) (prescient--save) (save-buffers-kill-emacs)))
            (evil-ex-define-cmd "Undotree" 'vundo)
            (evil-ex-define-cmd "k[ill]" 'kill-current-buffer)
            (evil-ex-define-cmd "trash" '(lambda () (interactive) (start-process-shell-command "rm" nil "rm -rf ~/.local/Trash")))
            
            (evil-set-initial-state 'dashboard-mode 'normal)
            
            (evil-add-command-properties #'consult-grep :jump t)
            
            (evil-define-operator ergo-word-delete (beg end type register yank-handler)
              "Delete word."
              :motion evil-a-word
              (evil-delete beg end type register yank-handler))
            
            (evil-define-operator ergo-word-change (beg end type register yank-handler)
              "Delete word."
              :motion evil-inner-word
              (evil-change beg end type register yank-handler))
            
            (general-def 'normal
              "I" 'evil-window-top
              "C-i" 'evil-goto-line
              "N" 'evil-window-bottom
              "C-n" 'evil-goto-first-line
              "E" 'evil-scroll-down
              "O" 'evil-scroll-up
              "x" 'evil-backward-WORD-begin
              "X" 'evil-backward-word-begin
              "C-x" 'evil-backward-WORD-end
              "j" 'evil-undo
              "J" 'evil-redo
              "a" 'evil-ex
              "r" 'evil-insert-line
              "R" 'evil-open-above
              "s" 'evil-append-line
              "S" 'evil-open-below
              "t" 'evil-insert
              "T" 'evil-append
              "C-t" 'evil-replace-state
              "u" 'evil-forward-WORD-begin
              "U" 'evil-forward-word-begin
              "C-u" 'evil-forward-WORD-end
              "-" 'evil-jump-backward
              "_" 'evil-jump-forward
              "m" 'evil-search-next
              "M" 'evil-search-previous
              "k" 'evil-delete-char
              "K" 'evil-substitute
              "l" 'evil-invert-char
              "L" 'evil-invert-case
              "c" 'evil-visual-char
              "C" 'evil-visual-line
              "C-c" 'evil-visual-block
              "v" 'evil-delete
              "V" 'evil-change
              "C-v" 'evil-delete-line
              "d" 'evil-yank
              "D" 'evil-yank-line
              "G" 'evil-paste-after
              ";" 'evil-end-of-visual-line
              ":" 'evil-end-of-line
              "C-;" 'evil-end-of-line
              "p" 'ergo-word-delete
              "P" 'ergo-word-change
              "C-p" 'ergo-word-change
              "$" 'evil-execute-macro
              "~" 'evil-record-macro
              "C-z" 'evil-goto-last-change-reverse
              "w" 'evil-repeat
              "W" 'evil-ex-repeat
              "C-w" 'evil-ex-repeat
              "l" 'evil-shift-right-line
              "L" 'evil-shift-left-line
              "C-l" 'evil-shift-left-line
              "y" 'evil-shift-right
              "Y" 'evil-shift-left
              "<escape>" 'doom/escape)
              
            
            (general-def 'motion
              "I" 'evil-window-top
              "C-i" 'evil-goto-line
              "N" 'evil-window-bottom
              "C-n" 'evil-goto-first-line
              "C-e" 'evil-scroll-page-down
              "C-o" 'evil-scroll-page-up
              "a" 'evil-ex
              "h" 'evil-set-marker
              "m" 'evil-search-next
              "M" 'evil-search-previous
              "-" 'evil-jump-backward
              "_" 'evil-jump-forward
              "/" 'isearch-forward-regexp
              "?" 'isearch-backward-regexp
              "f" 'evil-first-non-blank-of-visual-line
              "F" 'evil-beginning-of-visual-line
              "C-f" 'evil-first-non-blank
              "B" 'evil-goto-line)
            
            (general-swap-key nil '(motion normal visual)
              "g" "b"
              "z" "q"
              "Z" "Q")
            
            (general-def
              :keymaps 'override
              :states '(normal visual)
              "g" 'evil-paste-before
              "z" 'evil-jump-item
              "Z" 'evil-goto-last-change)
            
            (general-def
              :keymaps 'override
              :states '(operator visual)
              "i" 'evil-forward-char
              "s" evil-inner-text-objects-map
              "t" evil-outer-text-objects-map)
            
            (general-def 'visual
              "U" 'evil-forward-word-begin
              "u" 'evil-forward-WORD-begin
              "X" 'evil-backward-word-begin
              "x" 'evil-backward-WORD-begin
              "v" 'evil-delete-char
              "V" 'evil-substitute
              "C-v" 'evil-substitute
              "t" evil-outer-text-objects-map
              "s" evil-inner-text-objects-map
              "l" 'evil-invert-case
              "y" 'evil-shift-right
              "Y" 'evil-shift-left
              "C-t" 'evil-replace
              "R" 'evil-insert-line
              "C-r" 'evil-append-line
              "d" 'evil-yank-line
              "D" 'evil-yank-line
              "C-d" 'evil-yank-line
              "G" 'evil-paste)
            
            (general-def 'normal "bl" '(consult-goto-line :which-key "go to line")
              "b/" '(consult-keep-lines :which-key "delete non-matching lines"))
          '';
        };
      
        evil-collection.custom.evil-collection-unimpaired-want-repeat-mode-integration = true;
        
        evil-surround = {
          enable = true;
          deferIncrementally = true;
          config = ''
            (general-def 'visual evil-surround-mode-map "R" 'evil-surround-region)
            (general-def 'operator evil-surround-mode-map
              "s" nil
              "r" 'evil-surround-edit
              "R" 'evil-Surround-edit)
            (global-evil-surround-mode)
          '';
        };
        
        evil-easymotion = {
          enable = true;
          generalOne = {
            "'operator" = {
             "/" = "'evil-avy-goto-char-2"; 
             "?" = "'evil-avy-goto-char-2"; 
            };
            "(normal visual operator)" = {
              "H-m" = "'evilem-motion-search-next";
              "H-U" = "'evilem-motion-forward-word-begin";
              "H-u" = "'evilem-motion-forward-WORD-begin";
              "H-X" = "'evilem-motion-backward-word-begin";
              "H-x" = "'evilem-motion-backward-WORD-begin";
              "H-M" = "'evilem-motion-search-previous";
              "H-e" = "'evilem-motion-next-visual-line";
              "H-o" = "'evilem-motion-previous-visual-line";
            };
          };
          custom = {
            avy-dispatch-alist = ''
              '((?m . avy-action-cursor)
        			  (?l . avy-action-ispell)
        			  (?o . avy-action-embark)
        			  (?h . avy-action-helpful)
        			  (?g . avy-action-yank)
        			  (?p . avy-action-teleport)
        			  (?q . avy-action-fold))
            '';
            avy-keys = "'(?c ?r ?s ?t ?b ?f ?n ?e ?i ?a)";
          };
          config = ''
              ;; Stolen from karthink
              (defun avy-action-cursor (pt)
                (save-excursion
                  (goto-char pt)
                  (evil-mc-make-cursor-here))
                (select-window
                 (cdr (ring-ref avy-ring 0)))
                t)
            
              (defun avy-action-helpful (pt)
                (save-excursion
                  (goto-char pt)
                  (helpful-at-point))
                (select-window
                 (cdr (ring-ref avy-ring 0)))
                t)
            
              (defun avy-action-fold (pt)
                (save-excursion
                  (goto-char pt)
                  (evil-toggle-fold))
                (select-window
                 (cdr (ring-ref avy-ring 0)))
                t)
            
              (defun avy-action-embark (pt)
                (unwind-protect
                    (save-excursion
                      (goto-char pt)
                      (embark-act))
                  (select-window
                   (cdr (ring-ref avy-ring 0))))
                t)
          '';
        };
        
        evil-nerd-commenter = {
          enable = true;
          defer = true;
          generalOne."efs/leader-keys" = {
            "c" = '''(:ignore t :which-key "comment")'';
            "ci" = '''(evilnc-comment-or-uncomment-lines :which-key "comment line")'';
            "cl" = '''(evilnc-quick-comment-or-uncomment-paragraphs :which-key "comment paragraph")'';
            "cc" = '''(evilnc-copy-and-comment-lines :which-key "copy & comment lines")'';
            "cp" = '''(evilnc-comment-or-uncomment-paragraphs :which-key comment paragraph)'';
            "cr" = '''(comment-or-uncomment-region :which-key "comment region")'';
            "cv" = '''(evilnc-toggle-invert-comment-line-by-line :which-key "invert comments")'';
            "cy" = '''(evilnc-copy-and-comment-operator :which-key "copy & comment operator")'';
            "co" = '''(evilnc-comment-operator :which-key "copy operator")'';
          };
        };
        
        evil-mc = {
          enable = true;
          defer = true;
          command = ["evil-mc-pause-cursors" "evil-mc-make-cursor-here"];
          generalOne = {
            "'(normal visual)"."bz" = "'evil-mc-hydra/body";
            "efs/leader-keys"."C" = "'evil-mc-hydra/body";
          };
          config = ''
              (global-evil-mc-mode)
            
              (general-add-hook 'doom-escape-hook (lambda () (when (evil-mc-has-cursors-p)
              						 (evil-mc-undo-all-cursors)
              						 (evil-mc-resume-cursors) t)))
            
              ;; Don't mess with my macros.
              ;; https://github.com/gabesoft/evil-mc/issues/83
              (gsetq evil-mc-cursor-variables
                     (mapcar
              	(lambda (s)
              	  (remove 'register-alist
              		  (remove 'evil-markers-alist
              			  (remove evil-was-yanked-without-register s))))
              	evil-mc-cursor-variables))
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
      
          symex = {
            enable = true;
            defer = true;
            generalTwo."'normal"."(racket-repl-mode-map makefile-mode-map lisp-interaction-mode-map lisp-mode-map emacs-lisp-mode-map)"."RET" = "'symex-mode-interface";
            init = ''
                (with-eval-after-load 'evil-easymotion
                  (evilem-make-motion-plain evilem-symex-forward 'symex-traverse-forward :post-hook 'symex-select-nearest-in-line)
                  (evilem-make-motion-plain evilem-symex-backward 'symex-traverse-backward :post-hook 'symex-select-nearest-in-line)
                  (evilem-make-motion-plain evilem-symex-next-visual-line 'symex-next-visual-line :post-hook 'symex-select-nearest-in-line)
                  (evilem-make-motion-plain evilem-symex-previous-visual-line 'symex-previous-visual-line :post-hook 'symex-select-nearest-in-line)
                  (evilem-make-motion-plain evilem-symex-go-forward 'symex-go-forward :post-hook 'symex-select-nearest-in-line)
                  (evilem-make-motion-plain evilem-symex-go-backward 'symex-go-backward :post-hook 'symex-select-nearest-in-line))
                (gsetq symex--evil-keyspec
                       '(("n" . symex-go-backward)
                         ("e" . symex-go-down)
                         ("o" . symex-go-up)
                         ("i" . symex-go-forward)
                         ("be" . symex-next-visual-line)
                         ("bE" . evilem-symex-next-visual-line)
                         ("bO" . evilem-symex-previous-visual-line)
                         ("bo" . symex-previous-visual-line)
                         ("(" . symex-create-round)
                         ("[" . symex-create-square)
                         (")" . symex-wrap-round)
                         ("]" . symex-wrap-square)
                         ("C-'" . symex-cycle-quote)
                         ("C-," . symex-cycle-unquote)
                         ("`" . symex-add-quoting-level)
                         ("C-`" . symex-remove-quoting-level)
                         ("u" . symex-traverse-forward)
                         ("x" . symex-traverse-backward)
                         ("C-u" . evilem-symex-forward)
                         ("C-x" . evilem-symex-backward)
                         ("U" . symex-traverse-forward-skip)
                         ("X" . symex-traverse-backward-skip)
                         ("{" . symex-leap-backward)
                         ("}" . symex-leap-forward)
                         ("M-{" . symex-soar-backward)
                         ("M-}" . symex-soar-forward)
                         ("C-o" . symex-climb-branch)
                         ("C-e" . symex-descend-branch)
                         ("C-n" . evilem-symex-go-backward)
                         ("C-i" . evilem-symex-go-forward)
                         ("d" . symex-yank)
                         ("D" . symex-yank-remaining)
                         ("G" . symex-paste-after)
                         ("g" . symex-paste-before)
                         ("k" . symex-delete)
                         ("v" . symex-delete-backwards)
                         ("V" . symex-delete-remaining)
                         ("K" . symex-change)
                         ("C-v" . symex-change-remaining)
                         ("C--" . symex-clear)
                         ("s" . symex-replace)
                         ;; ("S" . symex-change-delimiter)
                         ("N" . symex-shift-backward)
                         ("I" . symex-shift-forward)
                         ("M-N" . symex-shift-backward-most)
                         ("M-I" . symex-shift-forward-most)
                         ("O" . paredit-raise-sexp)	; revisit kb
                         ("C-S-e" . symex-emit-backward)
                         ("C-(" . symex-capture-backward)
                         ("C-S-n" . symex-capture-backward)
                         ("C-{" . symex-emit-backward)
                         ("C-S-i" . symex-capture-forward)
                         ("C-}" . symex-emit-forward)
                         ("C-S-o" . symex-emit-forward)
                         ("C-)" . symex-capture-forward)
                         ("z" . symex-swallow)
                         ("Z" . symex-swallow-tail)
                         ("p" . symex-evaluate)
                         ("B" . symex-evaluate-remaining)
                         ("C-M-j" . symex-evaluate-pretty)
                         ("d" . symex-evaluate-definition)
                         ("M-j" . symex-eval-recursive)
                         ;; ("T". symex-evaluate-thunk)
                         ;; ("t" . symex-switch-to-scratch-buffer)
                         ("H" . symex-switch-to-messages-buffer)
                         ("l" . symex-repl)
                         ("L" . symex-run)
                         ("|" . symex-split)
                         ("&" . symex-join)
                         ("-" . symex-splice)
                         ("S" . symex-open-line-after)
                         ("R" . symex-open-line-before)
                         (">" . symex-insert-newline)
                         ("<" . symex-join-lines-backwards)
                         ("C->" . symex-append-newline)
                         ("C-<" . symex-join-lines)
                         ("C-S" . symex-append-newline)
                         ("E" . symex-join-lines)
                         ("M-E" . symex-collapse)
                         ("M-<" . symex-collapse)
                         ("M->" . symex-unfurl)
                         ("C-M-<" . symex-collapse-remaining)
                         ("C-M->" . symex-unfurl-remaining)
                         ("0" . symex-goto-first)
                         ("M-n" . symex-goto-first)
                         ("$" . symex-goto-last)
                         ("M-i" . symex-goto-last)
                         ("M-e" . symex-goto-lowest)
                         ("M-o" . symex-goto-highest)
                         ("=" . symex-tidy)
                         ("<tab>" . symex-tidy)
                         ("C-=" . symex-tidy-remaining)
                         ("C-<tab>" . symex-tidy-remaining)
                         ("M-=" . symex-tidy-proper)
                         ("M-<tab>" . symex-tidy-proper)
                         ("s" . symex-append-after)
                         ("T" . symex-insert-at-end)
                         ("t" . symex-insert-at-beginning)
                         ("r" . symex-insert-before)
                         ("w" . symex-wrap)
                         ("W" . symex-wrap-and-append)
                         ("C-d" . symex--evil-scroll-down)
                         (";" . symex-comment)
                         ("M-;" . symex-comment-remaining)
                         ("C-;" . symex-eval-print)	; weird pre-offset (in both)
                         ("s-;" . symex-evaluate)
                         ("H-h" . symex--toggle-highlight) ; treats visual as distinct mode
                         ("C-?" . symex-describe)
                         ("<return>" . symex-enter-lower)
                         ("<escape>" . symex-escape-higher)))    
            '';
            config = ''
                (gsetq symex-modal-backend 'evil)
                (symex-initialize)
                (repeaters-define-maps
                 '(("symex-visual-line"
                    symex-next-visual-line "e"
                    symex-previous-visual-line "o")))
                  
            '';
          } ;
 
          evil-god-state = {
            enable = true;
            defer = true;
            command = ["evil-god-state"];
            gfhook = ["('doom-escape-hook 'evil-god-state-bail)"];
            generalOne = {
              "'normal"."," = "'evil-execute-in-god-state";
              "'emacs"."<escape>" = "'evil-god-state";
              evil-god-state-map = {
                "<escape>" = "'evil-god-state-bail";
                "<return>" = "'evil-emacs-state";
              };
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
            "d" 'evil-yank)
          (evil-define-key 'visual 'evil-org-mode
            "i" 'evil-forward-char
            "s" evil-inner-text-objects-map)
        '';
        
        evil-org-agenda = {
          generalTwo."'motion".evil-org-agenda-mode-map = {
            "bn" = "'org-agenda-next-item";
            "bI" = "'evil-window-bottom";
            "I" = "'org-agenda-do-date-later";
            "C-S-i" = "'org-agenda-todo-nextset"; # Original binding "C-S-<right>"
            "l" = "'org-agenda-diary-entry";
          };
          generalOne."efs/leader-keys"."oa" = '''(org-agenda :which-key "agenda")'';
        };
        
        ewal-evil-cursors = {
          enable = true;
          demand = true;
          config = ''(ewal-evil-cursors-get-colors :apply t)'';
        };

          undo-fu = {
            enable = true;
            custom.undo-fu-session-compression = "'zst";
            afterCall = ["on-first-buffer-hook"];
            config = ''(gsetq evil-undo-system 'undo-fu)'';
          };
      };
    };
  }
