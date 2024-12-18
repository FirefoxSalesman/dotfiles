{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    libreoffice-fresh
    hunspell
    hunspellDicts.en-us-large
    pandoc
    unzip #for nov
  ];

  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      mode = [''("\\.org\\'" . org-mode)''];
      gfhook = ["('org-mode-hook '(efs/org-mode-setup my/org-capf))"];
      custom = {
        org-confirm-babel-evaluate = "nil";
        org-src-fontify-natively = "t";
        org-src-tab-acts-natively = "t";
        org-ellipsis = ''" ▾"'';
        org-log-done = "'time";
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
           (python . t)))
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
        org-modern-hide-stars = "t";
      };
    };
    
    org-modern-indent = {
      enable = true;
      package = epkgs: (pkgs.callPackage ./emacs-packages/org-modern-indent.nix {
        inherit inputs;
        inherit (epkgs) trivialBuild compat;
      });
      afterCall = ["org-mode-hook"];
      config = ''(general-add-hook 'org-mode-hook 'org-modern-indent-mode 90)'';
    };
    
    org-auto-tangle = {
      enable = true;
      ghook = ["('org-mode-hook 'org-auto-tangle-mode)"];
    };
    
    org-auto-export-pandoc = {
      enable = true;
      package = epkgs: (pkgs.callPackage ./emacs-packages/org-auto-export-pandoc.nix {
        inherit inputs;
        inherit (epkgs) trivialBuild ox-pandoc;
      });
      ghook = ["('after-save-hook (lambda () (when (equal major-mode 'org-mode) (org-auto-export-pandoc))))"];
    };

    denote = {
      enable = true;
      defer = true;
      gfhook = ["('dired-mode-hook 'denote-dired-mode-in-directories)"];
      custom = {
        denote-directory = ''(expand-file-name "~/doc/denote")'';
        denote-known-keywords = '''("quotes" "chem" "emacs" "java" "physics" "calculus" "minecraft" "de" "proofs" "csse230" "os" "cybercrime" "databases" "scifi" "software-requirements" "anthropology" "theoryofcomputation" "parallelcomp" "cybersecurity" "probstats" "scheme" "dreams" "softwaredevelopment" "ethics")'';
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

    tex = {
      enable = true;
      package = epkgs: epkgs.auctex;
      init = ''(setq-default TeX-master nil)'';
      gfhook = [
        ''
          ('LaTeX-mode-hook (list 'magic-latex-buffer
                                         'visual-line-mode
                                         'LaTeX-math-mode
                                         'flyspell-mode))
        ''
        ''
          ('reftex-load-hook (lambda ()
                                (gsetq reftex-section-levels
                                   (cons '("poemtitle" . -3) reftex-section-levels))))
        ''
      ];
      custom = {
        reftex-label-alist = '''(("\\poemtitle" ?P "poem:" "\\ref{%s}" nil ("poem" "poemtitle")))'';
        reftex-format-cite-function = ''
          '(lambda (key fmt)
             (let ((cite (replace-regexp-in-string "%l" key fmt))
                   (if ( or (= ?~ (string-to-char fmt))
                         (member (preceding-char) '(?\ ?\t |/n ?~)))
                       cite
                     (concat "~" cite)))))
        '';
        TeX-auto-save = "t";
        TeX-parse-self = "t";
        reftex-plug-into-AUCTeX = "t";
      };
      generalTwo.local-leader.LaTeX-mode-map = {
        "p" = '''(preview-at-point :which-key "preview")'';
        "a" = '''(eglot-code-actions :which-key "code actions")'';
        "n" = '''(flymake-goto-next-error :which-key "next error")'';
        "e" = '''(flymake-goto-prev-error :which-key "previous error")'';
        "f" = '''(eglot-format :which-key "format")'';
        "u" = '''(preview-clearout-at-point :which-key "unpreview")'';
      };
    };
    pdf-tools = {
      enable = true;
      defer = true;
      bindLocal.pdf-view-mode-map."C-s" = "search-forward";
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
    magic-latex-buffer = {
      enable = true;
      defer = true;
      afterCall = ["LaTeX-mode-hook"] ;
    };
    
    cdlatex = {
      enable = true;
      defer = true;
      ghook = [
        "('LaTeX-mode-hook 'turn-on-cdlatex)"
        "('org-mode-hook 'org-cdlatex-mode)"
      ];
      generalTwo."'insert" = {
        cdlatex-mode-map."TAB" = "'cdlatex-tab";
        org-cdlatex-mode-map."TAB" = "'cdlatex-tab";
      };
    };

    markdown = {
      enable = true;
      defer = true;
      bindLocal.markdown-mode-map."C-c C-e" = "markdown-do";
      gfhook = ["('markdown-mode-hook (list 'outline-minor-mode 'efs/markdown-font-setup))"];
      mode = [''("\\.md\\'" . gfm-mode)''];
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
          "od" = "'citar-denote-dwim";
          "oe" = "'citar-denote-open-reference-entry";
          "ok" = "'citar-denote-add-citekey";
          "oK" = "'citar-denoter-remove-citekey";
          "ol" = "'citar-denote-link-reference";
          "ob" = "'citar-denote-find-reference";
          "ow" = "'citar-denote-find-citation";
        };
        afterCall = ["citar"];
      };

    nov = {
      enable = true;
      defer = true;
      mode = [''("\\.epub\\'" . nov-mode)''];
    };
  };
}
