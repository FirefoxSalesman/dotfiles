{ config, inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    libreoffice-fresh
    hunspell
    hunspellDicts.en-us-large
    texlive.combined.scheme-full
  ];

  programs.emacs.init = {
    tools = {
      nov = true;
      pdf = true;
    };
    writing = {
      denote = true;
      citar = true;
    };
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
        gfhookf = [''('org-mode (list 'ispell-minor-mode
          (local! completion-at-point-functions (list (cape-capf-super
                                                       #'tempel-complete
      						 #'cape-file)
                                                      #'pcomplete-completions-at-point
      						#'cape-dict
                                                      #'cape-dabbrev))))''
        ];
        custom.org-directory = ''"~/doc"'';
        setopt = {
          org-export-with-section-numbers = false;
          org-export-with-toc = false;
          org-capture-templates = [
            '''("d" "Denote note" plain
      	(file denote-last-path)
              #'denote-org-capture
              :no-save t
              :immediate-finish nil
              :kill-buffer t
              :jump-to-captured t)''
            ''
              '("t" "Personal todo" entry
                                           (file org-default-todo-file)
                                           "* TODO [ ] %?\n%i\n%a" :prepend t)''
            ''
              '("n" "Personal notes" entry
                                           (file org-default-notes-file)
                                           "* %u %?\n%i\n%a" :prepend t)''
            ''
              '("j" "Journal" entry
                                           (file+olp+datetree org-default-journal-file)
                                           "* %U %?\n%i\n%a" :prepend t)''
      
            # Will use {project-root}/{todo,notes,changelog}.org, unless a
            # {todo,notes,changelog}.org file is found in a parent directory.
            # Uses the basename from `+org-capture-todo-file',
            # `+org-capture-changelog-file' and `+org-capture-notes-file'.
            '''("p" "Templates for projects")''
            ''
              '("pt" "Project-local todo" entry ; {project-root}/todo.org
                                           (file nix-emacs-project-todo)
                                           "* TODO %?\n%i\n%a" :prepend t)''
            ''
              '("pn" "Project-local notes" entry ; {project-root}/notes.org
                                           (file nix-emacs-project-notes)
                                           "* %U %?\n%i\n%a" :prepend t)''
            ''
              '("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
                                           (file+headline nix-emacs-project-changelog "Unreleased")
                                           "* %U %?\n%i\n%a" :prepend t)''
          ] ;
        };
        generalTwoConfig.local-leader.org-mode-map = {
          "a" = '''(avy-org-goto-heading-timer :which-key "avy")'';
          "e" = "'smart-export";
        };
      
        config = ''
          (require 'ol-man)
          (defun smart-export ()
            "Export the current buffer, according to its heading, then preview the result."
            (interactive)
            (let* ((export-type (cadr (assoc "EXPORT" (org-collect-keywords '("EXPORT")))))
          	 (result (cond ((equal export-type "pdf") (org-latex-export-to-pdf))
          		       ((equal export-type "odt")  (org-odt-export-to-odt))
                                 ((equal export-type "html") (when (< 2 (count-windows)) (elwm-split-window)) (eww-open-file (org-html-export-to-html)))))))
              (when (stringp result) (find-file-other-window result)))
        '' ;
      };

      org-auto-tangle = {
        enable = true;
        ghookf = ["('org-mode 'org-auto-tangle-mode)"];
      };
      
      denote.setopt.denote-known-keywords = [ ''"quotes"'' ''"chem"'' ''"emacs"'' ''"java"'' ''"physics"'' ''"calculus"'' ''"minecraft"'' ''"de"'' ''"proofs"'' ''"csse230"'' ''"os"'' ''"databases"'' ''"scifi"'' ''"softwarerequirements"'' ''"anthropology"'' ''"theoryofcomputation"'' ''"parallelcomp"'' ''"cybersecurity"'' ''"probstats"'' ''"scheme"'' ''"dreams"'' ''"softwaredevelopment"'' ''"ethics"'' ''"plp"'' ''"malwareanalysis"'' ''"bio"'' ''"ai"'' ''"resolve"'' ];
      
      biblio = {
        enable = true;
        generalOne.global-leader = {
          "ob" = '''(:ignore t :which-key "biblio")'';
          "obl" = "'biblio-lookup";
          "obi" = "'biblio-doi-insert-bibtex";
        };
      };
      
      pdf-tools = {
        generalOneConfig.pdf-view-mode-map."C-s" = "'search-forward";
        gfhookf = ["('pdf-view-mode 'pdf-view-midnight-minor-mode)"];
      };
      
      evil-collection-pdf = {
        defer = true;
        enable = true;
        generalTwoConfig.":n".pdf-view-mode-map = {
          "C-e" = "'pdf-view-scroll-up-or-next-page";
          "E" = "'pdf-view-scroll-up-or-next-page";
          "C-o" = "'pdf-view-scroll-down-or-previous-page";
          "O" = "'pdf-view-scroll-down-or-previous-page";
        };
      };
      
      cdlatex.generalTwoConfig.":i" = {
        cdlatex-mode-map."TAB" = "'cdlatex-tab";
        org-cdlatex-mode-map."TAB" = "'cdlatex-tab";
      };
      
      tex.generalTwoConfig.local-leader.LaTeX-mode-map."e" = "'TeX-command-run-all";
      
      evil-tex.generalOneConfig.":ov" = {
        "t" = "evil-tex-outer-text-objects-map";
        "s" = "evil-tex-inner-text-objects-map";
      };
      
      markdown = {
        generalOneConfig.markdown-mode-map."C-c C-e" = "'markdown-do";
        gfhookf = ["('markdown-mode 'efs/markdown-font-setup)"];
        setopt = {
          markdown-command = ''"multimarkdown"'';
          markdown-hide-markup = true;
        };
        generalTwoConfig = {
          ":n".markdown-mode-map = {
            "[h" = "'markdown-previous-visible-heading";
            "]h" = "'markdown-next-visible-heading";
          };  
        };
        preface = ''
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
        ghookf = ["((gen-mode-hooks '(Man org-agenda org Info markdown shrface)) 'writeroom-mode)"];
        gfhookf = ["('writeroom-mode 'visual-line-mode)"];
        setopt = {
          writeroom-mode-line = true;
          writeroom-maximize-window = false;
          writeroom-global-effects = false;
        };
        generalOne.global-leader."w" = '''(writeroom-mode :which-key "writeroom")'';
      };

      flyspell = {
        enable = true;
        ghookf = [
          "('text-mode 'flyspell-mode)"
          "('prog-mode 'flyspell-prog-mode)"
        ];
      };
      
      citar.setopt.citar-bibliography = [''"~/doc/uni.bib"''];
    };
  };
}
