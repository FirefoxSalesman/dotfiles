{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    libreoffice-fresh
    hunspell
    hunspellDicts.en-us-large
  ];

  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      symex = true;
      mode = [''("\\.org\\'" . org-mode)''];
      gfhook = ["('org-mode-hook '(efs/org-mode-setup my/org-capf))"];
      eglot = true;
      custom = {
        org-confirm-babel-evaluate = "nil";
        org-src-fontify-natively = "t";
        org-src-tab-acts-natively = "t";
        org-ellipsis = ''" ▾"'';
        org-log-done = "nil";
        org-log-into-drawer = "t";
        org-hide-emphasis-markers = "t";
        org-agenda-files = '''("~/doc/tasks.org")'';
        org-capture-templates = '''(("t" "Task" entry
                                      (file "~/doc/tasks.org")
                                      "* TODO %?\nDEADLINE: %^t"))'';
        org-emphasis-alist = '''(("*" bold)
                                   ("/" italic)
                                   ("=" org-verbatim verbatim)
                                   ("~" org-code verbatim)
                                   ("+" (:strike-through t))
                                   ("!" (:overline t) verbatim))'';
      };
      generalOne."efs/leader-keys" = {
        "o" = '''(:ignore t :which-key "org")'';
        "op" = '''org-capture'';
      };
      generalTwo.local-leader.org-mode-map = {
        "e" = '''(org-export-dispatch :which-key "export")'';
        "o" = '''(consult-org-heading :which-key "outline")'';
        "a" = '''(avy-org-goto-heading-timer :which-key "avy")'';
        "i" = '''(org-toggle-inline-images :which-key "show images")'';
        "b" = '''(org-edit-special :which-key "edit block")'';
      };
    
      config = ''
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((emacs-lisp . t )
           (python . t)
           (R . t)))
        (push '("conf-unix" . conf-unix) org-src-lang-modes)
        
        ;; This is needed as of Org 9.2
        (require 'org-tempo)
        
        (dolist (mode (list '("sh" . "src shell")
        		    '("sg" . "src sage")
        		    '("el" . "src emacs-lisp")
        		    '("cc" . "src C")
        		    '("cs" . "src css")
        		    '("hl" . "src html")
        		    '("js" . "src javascript")
        		    '("nx" . "src nix")
        		    '("jv" . "src java")
        		    '("py" . "src python")))
          (add-to-list 'org-structure-template-alist mode))
        
        (defun my/org-capf ()
          (setq-local completion-at-point-functions
                      (list (cape-capf-super
                             #'tempel-complete
                             #'cape-file)
        		    #'pcomplete-completions-at-point
        		    #'cape-dabbrev
        		    #'cape-dict)))
        
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
      init = ''
        ;; (add-to-list 'org-emphasis-alist '("‾" (:overline t)))
        
        (defun efs/org-font-setup ()
          ;; Replace list hyphen with dot
          (font-lock-add-keywords 'org-mode
                                  '(("^*\\([-]\\) "
                                     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
        
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
            (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face)))
        
          ;; Ensure that anything that should be fixed-pitch in Org files appears that way
          (dolist (face (list 'org-block 'org-table 'org-formula 'org-checkbox 'line-number 'line-number-current-line))
            (set-face-attribute face nil :inherit 'fixed-pitch))
          (dolist (face (list 'org-code 'org-table 'org-verbatim))
            (set-face-attribute face nil :inherit '(shadow fixed-pitch)))
          (dolist (face (list 'org-special-keyword 'org-meta-line))
            (set-face-attribute face nil :inherit '(font-lock-comment-face fixed-pitch))))
        
        (defun efs/org-mode-setup ()
          (org-indent-mode)
          (ispell-minor-mode)
          (org-toggle-pretty-entities)
          (variable-pitch-mode 1)
          (visual-line-mode 1)
          (efs/org-font-setup))
        
      '';
      deferIncrementally = ["calendar" "find-func" "format-spec" "org-macs" "org-compat" "org-faces" "org-entities" "org-list" "org-pcomplete" "org-src" "org-footnote" "org-macro" "ob" "org" "org-agenda" "org-capture" "evil-org-agenda"];
    };

    org-contrib = {
      enable = true;
      config = ''(ox-extras-activate '(ignore-headlines))'';
      deferIncrementally = ["ox-extra"];
    };
    
    org-modern = {
      enable = true;
      ghook = ["('org-mode-hook 'global-org-modern-mode)"];
      custom = {
        org-modern-star = "'replace";
        org-modern-hide-stars = "'leading";
      };
    };
    
    org-modern-indent = {
      enable = true;
      afterCall = ["org-mode-hook"];
      config = ''(general-add-hook 'org-mode-hook 'org-modern-indent-mode 90)'';
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
    
    org-appear = {
      enable = true;
      ghook = ["('org-mode-hook 'org-appear-mode)"];
    };
    
    denote = {
      enable = true;
      defer = true;
      gfhook = ["('dired-mode-hook 'denote-dired-mode-in-directories)"];
      custom = {
        denote-directory = ''(expand-file-name "~/doc/denote")'';
        denote-known-keywords = '''("quotes" "chem" "emacs" "java" "physics" "calculus" "minecraft" "de" "proofs" "csse230" "os" "databases" "scifi" "softwarerequirements" "anthropology" "theoryofcomputation" "parallelcomp" "cybersecurity" "probstats" "scheme" "dreams" "softwaredevelopment" "ethics" "plp")'';
        denote-file-type = "nil";
        denote-dired-directories = "(list denote-directory)";
        
      };
      generalOne."efs/leader-keys" = {
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
      generalOne."efs/leader-keys" = {
        "of" = '''(consult-notes :which-key "find note")'';
        "os" = '''(consult-notes-search-in-all-notes :which-key "search notes")'';
      };
    };
    
    tex.generalTwo.local-leader.LaTeX-mode-map = {
      "p" = '''(preview-at-point :which-key "preview")'';
      "a" = '''(eglot-code-actions :which-key "code actions")'';
      "n" = '''(flymake-goto-next-error :which-key "next error")'';
      "e" = '''(flymake-goto-prev-error :which-key "previous error")'';
      "f" = '''(eglot-format :which-key "format")'';
      "u" = '''(preview-clearout-at-point :which-key "unpreview")'';
    };
    
    pdf-tools = {
      enable = true;
      defer = true;
      generalOne.pdf-view-mode-map."C-s" = "'search-forward";
      custom = {
        # Makes PDFtools the default
        TeX-view-program-selection = '''((output-pdf "PDF Tools"))'';
        TeX-view-program-list = '''(("PDF Tools" TeX-pdf-tools-sync-view))'';
        TeX-source-correlate-start-server = "t";
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
    
    cdlatex.generalTwo."'insert" = {
      cdlatex-mode-map."TAB" = "'cdlatex-tab";
      org-cdlatex-mode-map."TAB" = "'cdlatex-tab";
    };
    
    markdown = {
      generalOne.markdown-mode-map."C-c C-e" = "'markdown-do";
      gfhook = ["('markdown-mode-hook 'efs/markdown-font-setup)"];
      custom = {
        markdown-command = ''"multimarkdown"'';
        markdown-hide-markup = "t";
      };
      generalTwo = {
        "'normal".markdown-mode-map = {
          "[h" = "'markdown-previous-visible-heading";
          "]h" = "'markdown-next-visible-heading";
        };  
        local-leader.markdown-mode-map = {
          "o" = '''(consult-outline :which-key "go to heading")'';
          "a" = '''(eglot-code-actions :which-key "code actions")'';
          "n" = '''(flymake-goto-next-error :which-key "next error")'';
          "e" = '''(flymake-goto-prev-error :which-key "previous error")'';
        };
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
      ghook = ["('(woman-mode-hook org-agenda-mode-hook org-mode-hook Info-mode-hook markdown-mode-hook) 'writeroom-mode)"];
      gfhook = ["('writeroom-mode-hook 'visual-line-mode)"];
      custom = {
        writeroom-mode-line = "t";
        writeroom-maximize-window = "nil";
        writeroom-global-effects = "nil";
      };
      generalOne."efs/leader-keys"."w" = '''(writeroom-mode :which-key "writeroom")'';
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
      generalOne."efs/leader-keys" = {
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
}
