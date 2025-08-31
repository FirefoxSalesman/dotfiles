  { config, inputs, pkgs, ... }:

  {
    home.packages = with pkgs; [
      libreoffice-fresh
      hunspell
      hunspellDicts.en-us-large
    ];

    programs.emacs.init = {
      ide.languages = {
        markdown.enable = true;
        latex = {
          enable = true;
          magicLatexBuffer = true;
          cdlatex = true;
        };
        org = {
          enable = true;
          aesthetics = {
            enable = true;
            headerFont = config.stylix.fonts.sansSerif.name;
          };
          captureTemplates.enable = true;
        };
      };

      usePackage = {
          org = {
            gfhook = ["('org-mode-hook '(ispell-minor-mode my/org-capf))"];
            custom = {
              org-export-with-toc = false;
              org-export-with-section-numbers = false;
              # org-agenda-files = '''("~/doc/tasks.org")'';
              org-directory = ''"~/doc"'';
              org-emphasis-alist = '''(("*" bold)
                                         ("/" italic)
                                         ("=" org-verbatim verbatim)
                                         ("~" org-code verbatim)
                                         ("+" (:strike-through t))
                                         ("!" (:overline t) verbatim))'';
            };
            generalTwo.local-leader.org-mode-map = {
              "o" = '''(consult-org-heading :which-key "outline")'';
              "a" = '''(avy-org-goto-heading-timer :which-key "avy")'';
            };
        
            config = ''
                (defun my/org-capf ()
                  (setq-local completion-at-point-functions
                              (list (cape-capf-super
                                     #'tempel-complete
                                     #'cape-file)
                		    #'pcomplete-completions-at-point
                		    #'cape-dabbrev
                		    #'cape-dict)))
            '';
          };

          org-auto-tangle = {
            enable = true;
            ghook = ["('org-mode-hook 'org-auto-tangle-mode)"];
          };
        
           org-auto-export-pandoc = {
             enable = true;
             extraPackages = with pkgs; [pandoc];
             ghook = ["('after-save-hook (lambda () (when (equal major-mode 'org-mode) (org-auto-export-pandoc))))"];
           };
        
        
          denote = {
            enable = true;
            defer = true;
            gfhook = ["('dired-mode-hook 'denote-dired-mode-in-directories)"];
            custom = {
              denote-directory = ''(expand-file-name "~/doc/denote")'';
              denote-known-keywords = '''("quotes" "chem" "emacs" "java" "physics" "calculus" "minecraft" "de" "proofs" "csse230" "os" "databases" "scifi" "softwarerequirements" "anthropology" "theoryofcomputation" "parallelcomp" "cybersecurity" "probstats" "scheme" "dreams" "softwaredevelopment" "ethics" "plp")'';
              denote-file-type = false;
              denote-dired-directories = "(list denote-directory)";
              
            };
            generalOne.global-leader = {
              "oc" = '''(denote :which-key "create note")'';
              "or" = '''(denote-rename-file :whick-key "denote rename")'';
              "oi" = '''(denote-link :which-key "link to note")'';
            };
          };
        
          consult-notes = {
            enable = true;
            defer = true;
            config = ''
                (with-eval-after-load 'embark
                  (defvar-keymap consult-notes-map
                    :doc "Keymap for Embark notes actions."
                    :parent embark-file-map)
                  (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))
                  ;; make embark-export use dired for notes
                  (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired))
              
                (when (locate-library "denote")
                  (consult-notes-denote-mode))
              
            '';
            generalOne.global-leader = {
              "of" = '''(consult-notes :which-key "find note")'';
              "os" = '''(consult-notes-search-in-all-notes :which-key "search notes")'';
            };
          };
        
          org-novelist = {
            enable = true;
            command = ["org-novelist-mode" "org-novelist-new-story"];
            generalTwo.local-leader.org-novelist-mode-map = {
              "c" = '''(:ignore t :which-key "character")'';
              "cn" = '''(org-novelist-new-character :which-key "new")'';
              "cr" = '''(org-novelist-rename-character :which-key "rename")'';
              "cd" = '''(org-novelist-destroy-character :which-key "destroy")'';
              "h" = '''(:ignore t :which-key "chapter")'';
              "hn" = '''(org-novelist-new-chapter :which-key "new")'';
              "hr" = '''(org-novelist-rename-chapter :which-key "rename")'';
              "hd" = '''(org-novelist-destroy-chapter :which-key "destroy")'';
              "p" = '''(:ignore t :which-key "place")'';
              "pn" = '''(org-novelist-new-place :which-key "new")'';
              "pr" = '''(org-novelist-rename-place :which-key "rename")'';
              "pd" = '''(org-novelist-destroy-place :which-key "destroy")'';
              "r" = '''(:ignore t :which-key "prop")'';
              "rn" = '''(org-novelist-new-prop :which-key "new")'';
              "rr" = '''(org-novelist-rename-prop :which-key "rename")'';
              "rd" = '''(org-novelist-destroy-prop :which-key "destroy")'';
              "e" = '''(org-novelist-export-story :which-key "export")'';
            };
          };
        
          pdf-tools = {
            enable = true;
            defer = true;
            generalOne.pdf-view-mode-map."C-s" = "'search-forward";
            custom = {
              # Makes PDFtools the default
              TeX-view-program-selection = '''((output-pdf "PDF Tools"))'';
              TeX-view-program-list = '''(("PDF Tools" TeX-pdf-tools-sync-view))'';
              TeX-source-correlate-start-server = false;
            };
            config = ''
                (pdf-tools-install)
                (with-eval-after-load 'evil-collection-pdf
                  (general-def 'normal pdf-view-mode-map
                    "C-e" 'pdf-view-scroll-up-or-next-page
                    "E" 'pdf-view-scroll-up-or-next-page
                    "C-o" 'pdf-view-scroll-down-or-previous-page
                    "O" 'pdf-view-scroll-down-or-previous-page))
            '';
            gfhook = [
              "('TeX-after-compilation-finished-functions #'TeX-revert-document-beffer)"
              "('pdf-view-mode-hook 'pdf-view-midnight-minor-mode)"];
            init = ''(setq-default pdf-view-display-size 'fit-width)'';
            extraConfig = '':magic ("%PDF" . pdf-view-mode)'';
          };
        
          cdlatex.generalTwo.":i" = {
            cdlatex-mode-map."TAB" = "'cdlatex-tab";
            org-cdlatex-mode-map."TAB" = "'cdlatex-tab";
          };
        
          markdown = {
            generalOne.markdown-mode-map."C-c C-e" = "'markdown-do";
            gfhook = ["('markdown-mode-hook 'efs/markdown-font-setup)"];
            custom = {
              markdown-command = ''"multimarkdown"'';
              markdown-hide-markup = true;
            };
            generalTwo = {
              ":n".markdown-mode-map = {
                "[h" = "'markdown-previous-visible-heading";
                "]h" = "'markdown-next-visible-heading";
              };  
              local-leader.markdown-mode-map."o" = '''(consult-outline :which-key "go to heading")'';
            };
            init = ''
                (defun efs/markdown-font-setup ()
                  (variable-pitch-mode)
                  (dolist (face '((markdown-header-face-1 . 1.4)
                                  (markdown-header-face-2 . 1.2)
                                  (markdown-header-face-3 . 1.1)
                                  (markdown-header-face-4 . 1.05)
                                  (markdown-header-face-5 . 1.05)
                                  (markdown-header-face-6 . 1.05)))
                    (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face))))
            '';
          };
        
          writeroom-mode = {
            enable = true;
            ghook = ["('(Man-mode-hook org-agenda-mode-hook org-mode-hook Info-mode-hook markdown-mode-hook) 'writeroom-mode)"];
            gfhook = ["('writeroom-mode-hook 'visual-line-mode)"];
            custom = {
              writeroom-mode-line = true;
              writeroom-maximize-window = false;
              writeroom-global-effects = false;
            };
            generalOne.global-leader."w" = '''(writeroom-mode :which-key "writeroom")'';
          };

          flyspell = {
            enable = true;
            custom.ispell-personal-dictionary = "~/.config/emacs/ispell.txt";
            ghook = [
              "('text-mode-hook 'flyspell-mode)"
              "('prog-mode-hook 'flyspell-prog-mode)"
            ];
          };
        
          citar = {
            enable = true;
            config = ''(citar-denote-mode)'';
            ghook = ["('(LaTeX-mode-hook org-mode-hook) 'citar-capf-setup)"];
            custom = {
              org-cite-insert-processor = "'citar";
              org-cite-follow-processor = "'citar";
              org-cite-activate-processor = "'citar";
              citar-bibliography = '''("~/doc/uni.bib")'';
              org-cite-global-bibliography = '''("~/doc/uni.bib")'';
            };
          };
        
          citar-embark = {
            enable = true;
            after = ["citar" "embark"];
            config = ''(citar-embark-mode)'';
            custom.citar-at-point-function = "'embark-act";
          };
        
          citar-denote = {
            enable = true;
            command = ["citar-denote-mode"];
            generalOne.global-leader = {
              "on" = '''(citar-create-note :which-key "new citar note")'';
              "oo" = '''(citar-denote-open-note :which-key "open citar note")'';
              "ol" = "'citar-denote-link-reference";
              "ow" = "'citar-denote-find-citation";
            };
            config = ''
                (defun citar-denote--create-note (citekey &optional _entry)
                  "Create a bibliographic note for CITEKEY with properties ENTRY.
              
                The note file type is determined by `citar-denote-file-type'.
              
                The title format is set by `citar-denote-title-format'.
              
                When `citar-denote-subdir' is non-nil, prompt for a subdirectory.
              
                When `citar-denote-template' is a symbol, use the specified
                template, if otherwise non-nil, prompt for a Denote template.
              
                When `citar-denote-signature' is non-nil, prompt for a signature or
                use citation key."
                  (denote
                   (read-string "Title: " (citar-denote--generate-title citekey))
                   (citar-denote--keywords-prompt citekey)
                   citar-denote-file-type
                   (when citar-denote-subdir
                     (if (stringp citar-denote-subdir)
                         (expand-file-name
                          (concat denote-directory citar-denote-subdir))
                       (denote-subdirectory-prompt)))
                   nil
                   (when citar-denote-template
                     (or (alist-get citar-denote-template denote-templates)
                         (denote-template-prompt)))
                   (cond ((eq citar-denote-signature 'ask)
                          (denote-signature-prompt nil "Signature: "))
                         ((eq citar-denote-signature 'citekey)
                          citekey)
                         (nil nil)))
                  (citar-denote--add-reference citekey)
                  ;; Open available atachment in other window
                  (when (one-window-p)
                    (split-window-right))
                  (other-window 1)
                  (citar-open-files citekey))
            '';
            afterCall = ["citar"];
          };

          nov = {
            enable = true;
            defer = true;
            extraPackages = with pkgs; [unzip];
            mode = [''("\\.epub\\'" . nov-mode)''];
          };
      };
    };
  }
