{ inputs, pkgs, pkgs-stable, config, ... }:

{
  imports = [./language-support];

  programs.emacs.init = {
    ide = {
      symex = true;  
      hoverDoc = true;
      eglot = {
        enable = true;
        preset = true;
      };
      languages = {
        bash.enable = true;
        gradle.enable = true;
        java.enable = true;
        json.enable = true;
        nix.enable = true;
        toml.enable = true;
        xml.enable = true;
        zenscript.enable = true;
        emacs-lisp = {
          enable = true;
          flymake = true;
        };
        org = {
          enable = true;
          aesthetics = {
            enable = true;
            headerFont = config.stylix.fonts.sansSerif.name;
          };
          evil = true;
        };
      };
    };

    usePackage = {
      editorconfig = {
        enable = true;
        afterCall = ["on-first-file-hook"];
        config = ''(editorconfig-mode)'';
      };
      
      rainbow-delimiters = {
        enable = true;
        ghook = ["('prog-mode-hook 'rainbow-delimiters-mode)"];
      };
      
      # lsp-mode.gfhook = ["('lsp-mode-hook (lambda () (company-mode -1)))"];
      # lsp-java.custom.lsp-java-content-provider-preferred = ''"fernflower"'';

      

      treesitter-context = {
        enable = true;
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
        gfhook = ["('eglot-managed-mode-hook 'my/eglot-capf)"];
        generalTwo.local-leader.eglot-mode-map = {
          "f" = "'eglot-format-buffer";
          "a" = "'eglot-code-actions";
          "d" = "'eldoc-doc-buffer";
        };
        config = ''
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
        generalTwo."local-leader".python-mode-map."r" = "'python-shell-send-buffer";
        custom = {
          python-shell-interpreter = ''"ipython"'';
          python-shell-interpreter-args = ''"-i --simple-prompt"'';
        };
      };
      
      code-cells.generalTwo = {
        "'normal".code-cells-mode-map = {
          "M-e" = "'code-cells-forward-cell";
          "M-o" = "'code-cells-backward-cell";
        };
        "local-leader".code-cells-mode-map = {
          "e" = "'code-cells-eval";
        };
      };
      
      json5-ts-mode = {
        enable = true;
        extraPackages = [pkgs.vscode-langservers-extracted];
        mode = [''"\\.json5\\'"''];
        eglot = true;
        symex = true;
        config = ''
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '((json5-ts-mode) . ("vscode-json-language-server" "--stdio"))))
        '';
      };

      racket-mode = {
        gfhook = ["('racket-mode-hook 'hs-minor-mode)"];
        generalTwo.local-leader.racket-mode-map = {
          "." = "'racket-xp-describe";
          "r" = "'racket-run";
        };
      };

      cider.generalTwo.local-leader.cider-mode-map."s" = '''(cider-jack-in :which-key "start cider")''; 

      java-ts-mode.init = ''
        (defun tkj/java-decompile-class ()
          "Run the FernFlower decompiler on the current .class file using
         fernflower, and opens the decompiled Java file."
          (interactive)
          (let* ((current-file (buffer-file-name))
                 (output-dir (concat (file-name-directory current-file) "decompiled/"))
                 (decompiled-file (concat output-dir (file-name-base current-file) ".java"))
                 (command (format "fernflower %s %s"
                                  (shell-quote-argument current-file)
                                  (shell-quote-argument output-dir))))
            (if (and current-file (string-equal (file-name-extension current-file) "class"))
                (progn
                  (unless (file-directory-p output-dir)
                    (make-directory output-dir t))
                  (message "Running FernFlower decompiler...")
                  (shell-command command)
                  (if (file-exists-p decompiled-file)
                      (find-file decompiled-file)
                    (message "Error: Decompiled file not found at %s" decompiled-file)))
              (message "Error: This command can only be run on .class files"))))
      '';

      prolog-mode.generalTwo."local-leader".prolog-mode-map."r" = '''(run-prolog :which-key "run")'';
    } ;
  };
}
