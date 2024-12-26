{ inputs, pkgs, ... }:

{
  home.packages = with pkgs; [
    python311Packages.python-lsp-server
    ccls
    vscode-langservers-extracted
    typescript-language-server
    lemminx
    marksman
    nodePackages.bash-language-server
    nixd
    sqls
  ];

  programs.emacs.init.usePackage = {
      nxml = {
        enable = true;
        generalTwo.local-leader.nxml-mode-map = {
          "a" = '''(eglot-code-actions :which-key "code actions")'';
          "n" = '''(flymake-goto-next-error :which-key "next error")'';
          "e" = '''(flymake-goto-prev-error :which-key "previous error")'';
          "f" = '''(eglot-format :which-key "format")'';
        };
        deferIncrementally = true;
      };
      
      editorconfig = {
        enable = true;
        afterCall = ["on-first-file-hook"];
        config = ''(editorconfig-mode)'';
      };
      
      rainbow-delimiters = {
        enable = true;
        ghook = ["('prog-mode-hook 'rainbow-delimiters-mode)"];
      };

      

      treesit-auto = {
        enable = true;
        custom.treesit-auto-install = "'prompt";
        deferIncrementally = ["treesitter"];
        config = ''
          (mp-setup-install-grammars)
          (global-treesit-auto-mode)
        '';
        extraConfig = ''
          :preface (defun mp-setup-install-grammars ()
                     "Install Tree-sitter grammars if they are absent."
                     (interactive)
                     (dolist (grammar
                              '(;(xml "https://github.com/ObserverOfTime/tree-sitter-xml")
                                (toml "https://github.com/ikatyang/tree-sitter-toml")
                                (elisp "https://github.com/Wilfred/tree-sitter-elisp")))
                       (add-to-list 'treesit-language-source-alist grammar)
                       ;; Only install `grammar' if we don't already have it
                       ;; installed. However, if you want to *update* a grammar then
                       ;; this obviously prevents that from happening.
                       (unless (treesit-language-available-p (car grammar))
                         (treesit-install-language-grammar (car grammar)))))
        '';
      };
      
      tree-sitter = {
        enable = true;
        afterCall = ["on-first-file-hook"];
        config = ''
          (global-tree-sitter-mode)
          (dolist (mode (list '(java-ts-mode . java)
            		  '(html-ts-mode . html)
            		  '(python-ts-mode . python)
            		  '(js-ts-mode . javascript)
            		  '(json-ts-mode . json)
            		  '(gfm-mode . markdown)
            		  '(css-ts-mode . css)
            		  '(c-ts-mode . c)
            		  '(racket-repl-mode . racket)))
            (add-to-list 'tree-sitter-major-mode-language-alist mode))
        '';
      };
      
      tree-sitter-langs = {
        enable = true;
        custom.tree-sitter-langs-grammar-dir = ''"~/.cache/emacs/tree-sitter"'';
        afterCall = ["global-tree-sitter-mode-hook"];
      };

      treesitter-context = {
        enable = true;
        package = epkgs: (epkgs.callPackage ./emacs-packages/treesitter-context.nix {
          inherit inputs;
          inherit (epkgs) trivialBuild posframe;
        });
        ghook = ["('(js-ts-mode-hook java-ts-mode-hook c-ts-mode-hook python-mode-hook json-ts-mode-hook) 'treesitter-context-mode)"];
        custom.treesitter-context-frame-min-width = "30";
      };
      
      treesitter-context-fold = {
        enable = true;
        hook = ["('treesitter-context-mode-hook 'treesitter-context-fold-mode)"];
        generalTwo."'normal".treesitter-context-fold-mode-map = {
          "zm" = "'treesitter-context-fold-hide";
          "zo" = "'treesitter-context-fold-show";
          "za" = "'treesitter-context-fold-toggle";
        };
      };

      magit = {
        enable = true;
        custom.magit-display-buffer-function = "#'magit-display-buffer-same-window-except-diff-v1";
        config = ''
          (defun dired-git-add ()
              (interactive)
              (start-process "git" nil "git" "add" (dired-get-marked-files)))
        '';
        generalOne.project-prefix-map = {
          "v" = "'magit-status";
          "c" = "'magit-commit";
          "p" = "'magit-pull";
          "P" = "'magit-push";
        };
      };
      
      project = {
        enable = true;
        generalOne."efs/leader-keys"."P" = "project-prefix-map";
      };
      
      projection-multi = {
        enable = true;
        generalOne.project-prefix-map."RET" = "'projection-multi-compile";
        config = ''
          (require 'projection)
          (global-projection-hook-mode)
          (oset projection-project-type-maven build "mvn -B clean compile")
        '' ;
      };
      
      projection-multi-embark = {
          enable = true;
          after = ["embark" "projection-multi"];
          config = ''(projection-multi-embark-setup-command-map)'';
      };

      devdocs = {
        enable = true;
        generalOne."efs/leader-keys"."hO" = "'devdocs-lookup";
      };

      plantuml-mode = {
        enable = true;
        mode = [''"\\.plantuml\\'"'' ''"\\.puml\\'"''];
        custom = {
          org-plantuml-exec-mode = "'plantuml";
          # plantuml-default-exec-mode = "'executable";
          # plantuml-executable-path = ''"${pkgs.plantuml}/bin/plantuml"'';
          org-plantuml-executable-path = ''"${pkgs.plantuml}/bin/plantuml"'';
        }; 
        config = ''
          (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
          
          (defun hex-encode (str)
            (string-join (mapcar (lambda (c) (format "%02x" c)) str)))
          
          (defun plantuml-server-encode-url (string)
            "Encode the string STRING into a URL suitable for PlantUML server interactions."
            (let* ((encoded-string (hex-encode string)))
              (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))
        '';
      };

      eglot = {
        enable = true;
        ghook = ["('(html-ts-mode-hook js-ts-mode-hook ess-r-mode-hook css-ts-mode-hook json-ts-mode-hook racket-mode-hook bibtex-mode-hook nxml-mode-hook nix-mode-hook markdown-mode-hook LaTeX-mode-hook bash-ts-mode-hook java-ts-mode-hook c-ts-mode-hook python-ts-mode-hook sql-mode-hook) 'eglot-ensure)"];
        gfhook = ["('eglot-managed-mode-hook 'my/eglot-capf)"];
        generalTwo.local-leader.eglot-mode-map = {
          "f" = "'eglot-format-buffer";
          "a" = "'eglot-code-actions";
        };
        config = ''
          (dolist (server (list '((nxml-mode) . ("lemminx"))
            		    '((html-ts-mode) . ("vscode-html-language-server" "--stdio"))
            		    '((sql-mode) . ("sqls"))))
            (add-to-list 'eglot-server-programs server))
          (defun my/eglot-capf ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'tempel-complete
                               #'eglot-completion-at-point
                               #'cape-file)
                              #'cape-dabbrev)))
          (general-add-advice 'evil-collection-eglot-setup
            :after '(lambda ()
                       (general-def 'normal eglot-mode-map "K" 'evil-substitute)))
        '';
      } ;
      
      eldoc-box = {
        enable = true;
        ghook = [
          "('(eglot-managed-mode-hook emacs-lisp-mode-hook) 'eldoc-box-hover-at-point-mode)"
          "('org-mode-hook 'eldoc-box-hover-mode)"
        ];
      };
      
      flymake = {
        enable = true;
        defer = true;
        config = ''(evil-ex-define-cmd "trouble" 'flymake-show-buffer-diagnostics)'';
      };
      
      eglot-tempel = {
        enable = true;
        after = ["eglot"];
        config = ''(eglot-tempel-mode)'';
      };
      
      breadcrumb = {
        enable = true;
        ghook = ["('(bibtex-mode-hook nxml-mode-hook nix-mode-hook racket-mode-hook markdown-mode-hook LaTeX-mode-hook bash-ts-mode-hook ess-r-mode-hook html-ts-mode-hook css-ts-mode-hook emacs-lisp-mode-hook) 'breadcrumb-local-mode)"];
      };

      # dape = {
      #   enable = true;
      #   after = ["eglot"];
      #   gfhook = ["('dape-on-stopped-hooks (list 'dape-info 'dape-repl))"];
      #   custom = {
      #     dape-window-arrangement = "gud";
      #     dape-key-prefix = ''"\C-x\C-a"'';
      #   };
      # };

      python-ts-mode = {
        enable = true;
        mode = [''"\\.py\\'"''];
        custom = {
          python-shell-interpreter = ''"ipython"'';
            python-shell-interpreter-args = ''"-i --simple-prompt"'';
        };
      };

      groovy-mode = {
        enable = true;
        mode = [''"\\.gradle\\'"''];
      };

      nix-mode = {
        enable = true;
        mode = [''"\\.nix\\'"''];
      };

      # html-ts-mode = {
      #   enable = true;
      #   mode = [''"\\.[px]?html?\\'"''];
      # };
      # 
      # emmet-mode = {
      #   enable = true;
      #   ghook = ["('(js-ts-mode-hook sgml-mode-hook css-ts-mode-hook html-ts-mode-hook) 'emmet-mode)"];
      #   custom.emmet-move-cursor-between-quotes = "t";
      # };
      # 
      # pug-mode = {
      #   enable = true;
      #   mode = [''"\\.pug\\'"''];
      # };

      # racket-mode = {
      #   enable = true;
      #   mode = [''"\\.rkt\\'"''];
      #   gfhook = ["('racket-mode-hook 'hs-minor-mode)"];
      #   init = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
      #   config = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
      #   generalTwo.local-leader.racket-mode-map = {
      #     "RET" = "'geiser-racket";
      #     "." = "'racket-xp-describe";
      #     "r" = "'racket-run";
      #   };
      # };

      ess-r-mode = {
        enable = true;
        package = epkgs: epkgs.ess;
        mode = [''"\\.R\\'"''];
        custom.ess-ask-for-ess-directory = "nil";
      };

      zenscript-mode = {
        enable = true;
        mode = [''"\\.zs\\'"''];
        # There's no way we're fixing the completion system, so we'll turn it off
        config = ''
          (defun zenscript-get-dumpzs (&optional prompt)
            "Returns nothing, because I can't fix the dumpfile problem"
            '(() . ()))
        '';
      };
  };
}
