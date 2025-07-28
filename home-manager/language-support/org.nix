  { pkgs, config, lib, ... }:

  let
    ide = config.programs.emacs.init.ide;
  in
  {
    options.programs.emacs.init.ide.languages.org = {
      enable = lib.mkEnableOption "Enables support for org mode. Borrows a great deal from emacs from scratch & doom emacs";
      aesthetics = {
        enable = lib.mkEnableOption "Enables org-modern & org-modern indent";
        headerFont = lib.mkOption {
          type = lib.types.str;
          default = "Liberation Serif";
          description = "The font to use for org headers. Defaults to Liberation Serif, since that's on most machines & I don't know what I'm doing";
        };
      };
    };

    config = lib.mkIf ide.languages.org.enable {
      programs.emacs.init.usePackage = {
        org = {
          enable = true;
          mode = [''("\\.org\\'" . org-mode)''];
          deferIncrementally = ["calendar" "find-func" "format-spec" "org-macs" "org-compat" "org-faces" "org-entities" "org-list" "org-pcomplete" "org-src" "org-footnote" "org-macro" "ob" "org" "org-agenda" "org-capture"];
          symex = ide.symex;
          custom = {
            org-confirm-babel-evaluate = lib.mkDefault false;
            org-src-fontify-natively = lib.mkDefault true;
            org-src-tab-acts-natively = lib.mkDefault true;
            org-log-done = lib.mkDefault false;
            org-log-into-drawer = lib.mkDefault true;
            org-hide-emphasis-markers = lib.mkIf ide.languages.org.aesthetics.enable (lib.mkDefault true);
            org-ellipsis = lib.mkIf ide.languages.org.aesthetics.enable (lib.mkDefault ''" ▾"'');

            # Settings from doom
            org-indirect-buffer-display = lib.mkDefault "'current-window";
            org-enforce-todo-dependencies = lib.mkDefault true;
            org-imenu-depth = lib.mkDefault "6";
            org-tags-column = lib.mkDefault "0";
            org-startup-folded  = lib.mkDefault false;
            org-agenda-inhibit-startup = lib.mkDefault true;
            org-agenda-window-setup = lib.mkDefault "'current-window";
            org-agenda-skip-unavailable-files = lib.mkDefault true;
            org-agenda-span = lib.mkDefault "10";
            org-agenda-start-on-weekday = lib.mkDefault false;
            org-agenda-start-day = lib.mkDefault ''"-3d"'';
            org-refile-targets = lib.mkDefault ''
              '((nil :maxlevel . 3)
                (org-agenda-files :maxlevel . 3))
            '';
            org-refile-use-outline-path = lib.mkDefault "'file";
            org-outline-path-complete-in-steps = lib.mkDefault false;
            org-src-preserve-indentation = lib.mkDefault true;
            org-link-elisp-confirm-function = lib.mkDefault false;
            org-src-window-setup = lib.mkDefault "'other-window";
            org-babel-lisp-eval-fn = lib.mkIf ide.languages.common-lisp.enable (lib.mkDefault "#'sly-eval");
            org-modules = lib.mkDefault "'(ol-bibtex)";
          };
          hook = ["(org-mode . org-indent-mode)"];
          config = ''
            (org-babel-do-load-languages
            'org-babel-load-languages
            '((emacs-lisp . t )
              (python . t)
              (R . t)))
            (push '("conf-unix" . conf-unix) org-src-lang-modes)
            ;; borrowed from doom
            (plist-put org-format-latex-options :scale 1.5)
          '';
        };

        org-appear = lib.mkIf ide.languages.org.aesthetics.enable {
          enable = true;
          hook = ["(org-mode . org-appear-mode)"];
        };

        org-contrib = lib.mkIf ide.hoverDoc {
          enable = true;
          config = ''(ox-extras-activate '(ignore-headlines))'';
          deferIncrementally = ["ox-extra"];
        };

        org-modern = lib.mkIf ide.languages.org.aesthetics.enable {
          enable = true;
          hook = [
            "(org-mode . org-modern-mode)"
            "(org-mode . org-toggle-pretty-entities)"
            "(org-mode . variable-pitch-mode)"
            "(org-mode . visual-line-mode)"
            "(org-mode . nix-emacs-org-font-setup)"
          ];
          custom = {
            org-modern-star = "'replace";
            org-modern-hide-stars = "'leading";
          };
          init = ''
            (defun nix-emacs-org-font-setup ()
              "Sets up org fonts. Originates from Emacs from Scratch."
              ;;Set faces for heading levels
              (dolist (face '((org-document-title . 1.4)
                              (org-level-1 . 1.4)
                              (org-level-2 . 1.3)
                              (org-level-3 . 1.2)
                              (org-level-4 . 1.1)
                              (org-level-5 . 1.05)
                              (org-level-6 . 1.05)
                              (org-level-7 . 1.05)
                              (org-level-8 . 1.05)))
                      (set-face-attribute (car face) nil :font "${ide.languages.org.aesthetics.headerFont}" :weight 'regular :height (cdr face)))

              ;; Ensure that anything that should be fixed-pitch in Org files appears that way
              (dolist (face (list 'org-block 'org-table 'org-formula 'org-checkbox 'line-number 'line-number-current-line))
                (set-face-attribute face nil :inherit 'fixed-pitch))
              (dolist (face (list 'org-code 'org-table 'org-verbatim))
                (set-face-attribute face nil :inherit '(shadow fixed-pitch)))
              (dolist (face (list 'org-special-keyword 'org-meta-line))
                (set-face-attribute face nil :inherit '(font-lock-comment-face fixed-pitch))))
          '';
        };

        eldoc-box = lib.mkIf ide.hoverDoc {
          enable = true;
          hook = ["(org-mode . eldoc-box-hover-mode)"];
        };

        org-modern-indent = lib.mkIf ide.languages.org.aesthetics.enable {
          enable = true;
          afterCall = ["org-mode-hook"];
          config = ''(add-hook 'org-mode-hook 'org-modern-indent-mode 90)'';
        };

        evil-org = lib.mkIf ide.evil {
          enable = true;
          ghook = ["('org-mode-hook 'evil-org-mode)"];
          gfhook = ["('org-capture-mode-hook 'evil-insert-state)"];
          # stolen from doom
          generalTwo."'normal".org-mode-map = {
            "]h" = '''(org-forward-heading-same-level :which-key "next heading")'';
            "[h" = '''(org-backward-heading-same-level :which-key "prev heading")'';
            "]c" = '''(org-babel-next-src-block :which-key "next src block")'';
            "[c" = '''(org-babel-previous-src-block :which-key "prev src block")'';
            "]l" = '''(org-next-link :which-key "next link")'';
            "[l" = '''(org-previous-link :which-key "prev link")'';
          };
          init = ''
            (add-to-list 'evil-fold-list
  	                     `((org-mode)
  	                       :open org-cycle
  	                       :open-all nil
  	                       :close org-cycle
  	                       :close-all nil
  	                       :toggle org-cycle
  	                       :delete nil
  	                       :open-rec nil))        
          '';
          config = ''
            (with-eval-after-load 'evil-collection
              (when (equal evil-collection-unimpaired-want-repeat-mode-integration t)
                (evil-collection-unimpaired-defvar-keymap org-forward-heading-same-level-repeat-map
                  "h" #'org-forward-heading-same-level
                  "H" #'org-backward-heading-same-level)
                (evil-collection-unimpaired-defvar-keymap org-backward-heading-same-level-repeat-map
                  "h" #'org-backward-heading-same-level
                  "H" #'org-forward-heading-same-level)
                (evil-collection-unimpaired-defvar-keymap org-babel-next-src-block-repeat-map
                  "c" #'org-babel-next-src-block
                  "C" #'org-babel-previous-src-block)
                (evil-collection-unimpaired-defvar-keymap org-babel-previous-src-block-repeat-map
                  "c" #'org-babel-previous-src-block
                  "C" #'org-babel-next-src-block)
                (evil-collection-unimpaired-defvar-keymap org-next-link-repeat-map
                  "l" #'org-next-link
                  "L" #'org-previous-link)
                (evil-collection-unimpaired-defvar-keymap org-previous-link-repeat-map
                  "l" #'org-previous-link
                  "L" #'org-next-link)
                (dolist (cmd '(org-forward-heading-same-level
                               org-backward-heading-same-level
                               org-babel-next-src-block
                               org-babel-previous-src-block
                               org-next-link
                               org-previous-link))
                        (put cmd 'repeat-map (intern (format "%s-repeat-map" cmd))))))
          '';
        };

        evil-org-agenda = lib.mkIf ide.evil {
          enable = true;
          config = ''(evil-org-agenda-set-keys)'';
          deferIncrementally = true;
        };
      };
    };
  }
