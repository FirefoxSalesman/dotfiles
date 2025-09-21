{ config, pkgs, lib, ... }:

{
  programs.emacs.init = {
    ide = {
      project = true;
      flymake = {
        enable = true;
        preset = true;
      };
      symex = true;  
      hoverDoc = true;
      eglot = {
        enable = true;
        preset = true;
      };
      direnv = true;
      languages = {
        bash.enable = true;
        gradle.enable = true;
        java = {
          enable = true;
          moreEglot = true;
        };
        json.enable = true;
        nix.enable = true;
        toml.enable = true;
        xml.enable = true;
        zenscript.enable = true;
        emacs-lisp.enable = true;
        javascript.enable = true;
        ledger.enable = true;
	c.enable = true;
	r.enable = true;
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
      
      # lsp-java.custom.lsp-java-content-provider-preferred = ''"fernflower"'';
      

      treesit-fold = {
        enable = true;
        ghook = ["((gen-mode-hooks '(bash-ts c-ts css-ts emacs-lisp erlang-ts go-ts haskell-ts html-ts java-ts js-ts json-ts json5-ts julia-ts kotlin-ts lua-ts make nix-ts python-ts ess-r rustic scala-ts svelte-ts swift-ts toml-ts typescript-ts vimscript-ts yaml-ts zig-ts)) 'treesit-fold-mode)"];
      };

      magit = {
        enable = true;
        custom.magit-display-buffer-function = "#'magit-display-buffer-same-window-except-diff-v1";
        generalOne.project-prefix-map = {
          "v" = "'magit-status";
          "c" = "'magit-commit";
          "p" = "'magit-pull";
          "P" = "'magit-push";
          "b" = "'magit-branch";
          "m" = "'magit-merge";
        };
      };
      
      projection-ibuffer = {
        enable = true;
        generalOne.project-prefix-map.i = "(cmd! () (ibuffer) (ibuffer-filter-by-projection-root (project-current)))";
      };
      
      projection-multi.custom.projection-gradle-use-daemon = false;
      
      projection-multi-embark = {
        enable = true;
        after = ["embark" "projection-multi"];
        config = ''(projection-multi-embark-setup-command-map)'';
      };

      eglot = {
        gfhook = [
          ''('eglot-managed-mode-hook (local! completion-at-point-functions (list (cape-capf-super #'tempel-complete
                                                                                                   #'eglot-completion-at-point
      											     #'cape-file)
                                                                                  #'cape-dabbrev)))''
          "('before-save-hook (lambda () (when eglot--managed-mode (eglot-format-buffer))))"
        ];
        config = ''
          (general-add-advice 'evil-collection-eglot-setup
          		    :after '(lambda ()
          			      (general-def 'normal eglot-mode-map "K" 'evil-substitute)))
        '';
        generalTwo.local-leader.eglot-mode-map."r" = "'eglot-rename";
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

      python-ts-mode.custom = {
          python-shell-interpreter = ''"ipython"'';
          python-shell-interpreter-args = ''"-i --simple-prompt"'';
      };
      
      make-mode = {
        enable = true;
        symex = true;
        ghook = ["('makefile-mode-hook (treesit! 'make))"];
      };

      yaml-ts-mode = {
        enable = true;
        mode = [''"\\.yaml\\'"''];
        extraPackages = [pkgs.yaml-language-server];
        symex = true;
        eglot = ''("yaml-language-server" "--stdio")'';
      };
      
      racket-mode.gfhook = ["('racket-mode-hook 'hs-minor-mode)"];

      elisp-mode.gfhook = ["('emacs-lisp-mode-hook (local! completion-at-point-functions (list (cape-capf-super 'tempel-complete 'elisp-completion-at-point))))"];
      java-ts-mode = {
        init = ''
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
      };
    };
  };
}
