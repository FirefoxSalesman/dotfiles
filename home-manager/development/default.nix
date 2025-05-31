{ inputs, pkgs, pkgs-stable, ... }:

{
  imports = [
    # ./clojure.nix
    # ./python.nix
    ./java.nix
    ./nix.nix
    # ./web-development.nix
    ./json.nix
    ./toml.nix
    # ./racket.nix
    # ./haskell.nix
    # ./c.nix
    ./bash.nix
    # ./r.nix
    # ./jupyter.nix
    # ./prolog.nix
    ./zenscript.nix
    # ./rust.nix
    # ./lua.nix
    # ./plantuml.nix
    # ./scala.nix
    # ./erlang.nix
    # ./sql.nix
    # ./forth.nix
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
        eglot = true;
        symex = true;
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
          		    '(lua-ts-mode . lua)
          		    '(python-ts-mode . python)
          		    '(scala-ts-mode . scala)
          		    '(js-ts-mode . javascript)
          		    '(json-ts-mode . json)
          		    '(gfm-mode . markdown)
          		    '(rust-ts-mode . rust)
          		    '(css-ts-mode . css)
          		    '(c-ts-mode . c)
          		    '(racket-repl-mode . racket)
          		    '(ess-r-mode . r)
          		    '(inferior-ess-r-mode . r)
          		    '(erlang-ts-mode . erlang)
          		    '(toml-ts-mode . toml)))
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
        package = epkgs: (epkgs.callPackage ../emacs/emacs-packages/treesitter-context.nix {
          inherit inputs;
          inherit (epkgs) trivialBuild posframe;
        });
        ghook = ["('(js-ts-mode-hook haskell-mode java-ts-mode-hook rustic-mode-hook c-ts-mode-hook python-mode-hook json-ts-mode-hook) 'treesitter-context-mode)"];
        custom.treesitter-context-frame-min-width = "30";
        config = ''
          (dolist (treesit-support '(treesitter-context--supported-mode treesitter-context--focus-supported-mode  treesitter-context--fold-supported-mode))
                  (add-to-list treesit-support 'rustic-mode)
                  (add-to-list treesit-support 'haskell-mode))
        '';
      };
      
      treesitter-context-fold = {
        enable = true;
        ghook = ["('treesitter-context-mode-hook 'treesitter-context-fold-mode)"];
        generalTwo."'normal".treesitter-context-fold-mode-map = {
          "zm" = "'treesitter-context-fold-hide";
          "zo" = "'treesitter-context-fold-show";
          "za" = "'treesitter-context-fold-toggle";
        };
      };
      
      treesitter-context-focus = {
        enable = true;
        command = ["treesitter-context-focus-mode"];
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
          "b" = "'magit-branch";
          "m" = "'magit-merge";
        };
      };
      
      project = {
        enable = true;
        generalOne."efs/leader-keys"."P" = "project-prefix-map";
        custom.project-vc-extra-root-markers = '''("Cargo.toml")'';
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

      eglot = {
        enable = true;
        # ghook = ["('LaTeX-mode-hook 'eglot-ensure)"];
        gfhook = ["('eglot-managed-mode-hook 'my/eglot-capf)"];
        generalTwo.local-leader.eglot-mode-map = {
          "f" = "'eglot-format-buffer";
          "a" = "'eglot-code-actions";
          "d" = "'eldoc-doc-buffer";
        };
        config = ''
          (dolist (server (list '((nxml-mode) . ("lemminx"))
          		      '((scala-ts-mode) . ("metals"))
          		      '((html-ts-mode) . ("vscode-html-language-server" "--stdio"))
          		      '((lua-ts-mode) . ("lua-language-server"))
          		      '((rust-ts-mode rust-mode) .
          			("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
          		'((sql-mode) . ("sqls")))
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
      
      # breadcrumb = {
      #   enable = true;
      #   ghook = ["('(bibtex-mode-hook nxml-mode-hook nix-mode-hook racket-mode-hook markdown-mode-hook LaTeX-mode-hook bash-ts-mode-hook ess-r-mode-hook html-ts-mode-hook css-ts-mode-hook emacs-lisp-mode-hook) 'breadcrumb-local-mode)"];
      # };
      
      # dape = {
      #   enable = true;
      #   after = ["eglot"];
      #   gfhook = ["('dape-on-stopped-hooks (list 'dape-info 'dape-repl))"];
      #   custom = {
      #     dape-window-arrangement = "gud";
      #     dape-key-prefix = ''"\C-x\C-a"'';
      #   };
      # };
  };
}
