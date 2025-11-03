{ config, pkgs, lib, ... }:

{
  programs.emacs.init = {
    completions.tempel.templates = {
      nix-ts-mode.upackage = ''p "= {" n "  enable = true;" q "  }"'';
      ledger-mode = {
	payroll = ''p "Rose-Hulman Payroll" n> "Income:TeachingAssistant" > "-" p n> "*Assets:Checking"'';
	checking = ''"Assets:Checking"'';
      };
      emacs-lisp-mode = {
	wcd = ''"(with-current-buffer " "q)"'';
	gbc = ''"(get-buffer-create " "q)"'';
      };
    };
    ide = {
      project = true;
      copilot = {
	enable = true;
	keepOutOf = ["c-ts-mode" "json5-ts-mode" "json-ts-mode" "LaTeX-mode"];
      };
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
	python.enable = true;
	r.enable = true;
	c.enable = true;
	makefile.enable = true;
	yaml.enable = true;
      };
    };

    usePackage = {
      rainbow-delimiters = {
        enable = true;
        ghookf = ["('prog-mode 'rainbow-delimiters-mode)"];
      };
      
      # lsp-java.setopt.lsp-java-content-provider-preferred = ''"fernflower"'';
      

      treesit-fold = {
        enable = true;
        ghookf = ["((gen-mode-hooks '(bash-ts c-ts css-ts emacs-lisp erlang-ts go-ts haskell-ts html-ts java-ts js-ts json-ts json5-ts julia-ts kotlin-ts lua-ts make nix-ts python-ts ess-r rustic scala-ts svelte-ts swift-ts toml-ts typescript-ts vimscript-ts yaml-ts zig-ts)) 'treesit-fold-mode)"];
      };

      magit = {
        enable = true;
        setopt = {
          magit-display-buffer-function = "#'magit-display-buffer-same-window-except-diff-v1";
          magit-process-find-password-functions = ["'magit-process-password-auth-source"];
        };
        generalOne.project-prefix-map = {
          "v" = "'magit-status";
          "c" = "'magit-commit";
          "p" = "'magit-pull";
          "P" = "'magit-push";
          "b" = "'magit-branch";
          "m" = "'magit-merge";
        };
      };
      
      magit-todos = {
        enable = true;
        after = ["magit"];
        config = "(magit-todos-mode)";
      };
      
      projection-ibuffer = {
        enable = true;
        generalOne.project-prefix-map.i = ''`("ibuffer" . ,(cmd! (ibuffer) (ibuffer-filter-by-projection-root (project-current))))'';
      };
      
      projection-multi.custom.projection-gradle-use-daemon = false;
      
      projection-multi-embark = {
        enable = true;
        after = ["embark" "projection-multi"];
        config = "(projection-multi-embark-setup-command-map)";
      };

      eglot = {
        preface = "(defvar efs/autoformat t)";
        gfhookf = [
          ''('eglot-managed-mode (local! completion-at-point-functions (list (cape-capf-super #'tempel-complete
            #'eglot-completion-at-point
      											     #'cape-file))))
          ''
          "('before-save (lambda () (when (and eglot--managed-mode efs/autoformat) (eglot-format-buffer))))"
        ];
        config = ''
          (efs/evil-collection-remap 'evil-collection-eglot-setup 'normal eglot-mode-map 
          			   "K" 'evil-substitute)
        '';
      };
      
      eglot-java = {
        setopt.eglot-java-user-init-opts-fn = "'eglot-java-init-opts";
        preface = ''
          (defun eglot-java-init-opts (server eglot-java-eclipse-jdt)
              '(:bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]))
        '';
      };
      
      dape = {
        enable = true;
        after = ["eglot"];
        gfhookf = ["('dape-on-stopped (list 'dape-info 'dape-repl))"];
        setopt = {
          dape-window-arrangement = "'gud";
          dape-key-prefix = ''"\C-x\C-a"'';
        };
      };
      
      projection-dape = {
        enable = true;
        after = ["dape"];
        generalOne.project-prefix-map."d" = "'projection-dape";
      };

      python-ts-mode.setopt = {
        python-shell-interpreter = ''"ipython"'';
        python-shell-interpreter-args = ''"-i --simple-prompt"'';
      };

      yara-mode = {
        enable = true;
        mode = [''"\\.yar\\'"''];
      };
      
      racket-mode.gfhookf = ["('racket-mode 'hs-minor-mode)"];

      elisp-mode.gfhookf = ["('emacs-lisp-mode (local! completion-at-point-functions (list (cape-capf-super 'tempel-complete 'elisp-completion-at-point))))"];

      flymake-popon.setopt.flymake-popon-posframe-extra-arguments = [ "':poshandler" "'posframe-poshandler-point-bottom-left-corner-upward"
	"':parent-frame" false
        "':refposhandler" "'posframe-refposhandler-xwininfo" ];

      eat.generalOne.global-leader.a = ''`("AI via cursor" . ,(cmd! (eat "${pkgs.cursor-cli}/bin/cursor-agent")))'';

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
