{ config, pkgs, inputs, ... }:

{
  imports = [
    ./emacs/early-init.nix
    ./emacs/evil.nix
    ./emacs/file-management.nix
    ./emacs/help-system.nix
    ./emacs/completion-system.nix
    ./emacs/passwords.nix
    ./emacs/writing.nix
    ./emacs/window-manager.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-gtk3;
    extraPackages = epkgs: with epkgs; [ 
      on
      repeaters
      hydra
      pretty-hydra
    ];
    init = {
      enable = true;
      packageQuickstart = false;
      recommendedGcSettings = true;
      usePackageVerbose = false;
      largeFileHandling = true;

      prelude =''
        (defalias 'gsetq #'general-setq)
        
        (use-package on
          :demand t)
        
        (general-create-definer efs/leader-keys
          :keymaps 'override
          :states '(emacs insert normal hybrid motion visual operator)
          :prefix "SPC"
          :global-prefix "C-SPC")
        
        (efs/leader-keys
          "f" '(find-file :which-key "find or create file")
          ;; Help
          "h" '(:ignore t :which-key "help")
          "h." '(display-local-help :which-key "display local help")
          "h4" '(info-other-window :which-key "info other window")
          "hA" '((lambda () (interactive) (async-shell-command "${pkgs.wiki}/bin/wiki")) :which-key "arch wiki")
          "hC" '(describe-coding-system :which-key "describe coding system")
          "hD" '(view-emacs-debugging :which-key "emacs debugging")
          "hE" '(view-external-packages :which-key "external packages")
          "hF" '(Info-goto-emacs-command-node :which-key "info: command node")
          "hb" '(embark-bindings :which-key "display all keybinds")
          "hI" '(describe-input-method :which-key "describe input method")
          "hK" '(Info-goto-emacs-key-command :which-key "info: key command")
          "hL" '(describe-language-environment :which-key "describe language environment")
          "hM" '(woman :which-key "man")
          "hP" '(describe-package :which-key "describe package")
          "hR" '(info-display-manual :which-key "info: manual")
          "hS" '(info-lookup-symbol :which-key "info: symbol")
          "hT" '(view-emacs-todo :which-key "things you can do to help emacs")
          "ha" '(about-emacs :which-key "about emacs")
          "hc" '(describe-key-briefly :which-key "short describe key")
          "hd" '(apropos-documentation :which-key "apropos documentation")
          "he" '(view-echo-area-messages :which-key "view echoed messages")
          ;; "hf" '(describe-function :which-key "describe function")
          "hh" '(help-for-help :which-key "help for help")
          "hi" '(info :which-key "info pages")
          ;; "hk" '(describe-key :which-key "describe key")
          "hl" '(view-lossage :which-key "lossage")
          "hm" '(describe-mode :which-key "describe mode")
          "hn" '(view-emacs-news :which-key "emacs news")
          ;; "ho" '(describe-symbol :which-key "describe symbol")
          "hp" '(finder-by-keyword :which-key "finder by keyword")
          "hq" '(help-quit :which-key "help quit")
          "hr" '(info-emacs-manual :which-key "info: emacs")
          "hs" '(describe-syntax :which-key "describe syntax")
          "ht" '(help-with-tutorial :which-key "emacs tutor")
          ;; "hv" '(describe-variable :which-key "describe variable")
          "hw" '(where-is :which-key "find binds of command")
          ;; "hx" '(describe-command :which-key "describe command")
          "h C-f" '(view-emacs-FAQ :which-key "emacs FAQ")
          "h C-p" '(view-emacs-problems :which-key "view emacs problems")
          "h C-s" '(search-forward-help-for-help :which-key "search in help for help")
          ;;Mouse
          "l" '(compile :which-key "compile")
          ;; Mount/unmount drive
          "u" '((lambda () (interactive) (start-process-shell-command "udisksmenu" nil "${pkgs.udisksmenu}/bin/udisksmenu")) :which-key "mount/unmount drive"))
        
        (general-unbind "C-h")
        
        (general-create-definer local-leader
          :prefix "M-SPC"
          :states '(emacs insert normal hybrid motion visual operator))
        
        (use-package hydra
          :defer t)
        
        (use-package pretty-hydra
          :demand t
          :custom
          (hydra-hint-display-type 'posframe)
          :config
          (gsetq hydra-posframe-show-params '(:internal-border-width 1
        							     :internal-border-color "003f28"
        							     :parent-frame nil
        							     :poshandler posframe-poshandler-frame-bottom-center
        							     :refposhandler posframe-refposhandler-xwininfo))
          :gfhook ('doom-escape-hook 'hydra-keyboard-quit))
        
        (use-package repeaters
          :demand t
          :config
          (repeaters-define-maps
           '(("next-error" ;; borrowed from the hydra wiki
              next-error "`"
              next-error "n"
              previous-error "e"))))
        
        (use-package repeat
          :config
          (repeat-mode))
        
        (general-def "H-z" 'repeat)
      '';

      usePackage = {

        tooltip = {
          enable = true;
          config = ''
            (tooltip-mode -1)
            (set-fringe-mode -1)
          '';
        };
        
        simple = {
          enable = true;
          config = ''
            (gsetq save-interprogram-paste-before-kill t)
            (column-number-mode)
          '';
        };
        
        display-line-numbers = {
          enable = true;
          custom = {
            display-line-numbers-type = "'relative";
            display-line-numbers-width = "3";
          }; 
          config = "(global-display-line-numbers-mode)";
          #Disable line numbers for some modes
          ghook = ["('(org-mode-hook term-mode-hook dired-mode-hook eww-mode-hook eat-mode-hook markdown-mode-hook help-mode-hook helpful-mode-hook Info-mode-hook woman-mode-hook shell-mode-hook pdf-view-mode-hook elfeed-search-mode-hook elfeed-show-mode-hook eshell-mode-hook racket-repl-mode-hook sage-shell-mode-hook) (lambda () (display-line-numbers-mode 0)))"];
        } ;
        
        elec-pair = {
          enable = true;
          ghook = ["('on-first-buffer-hook 'electric-pair-mode)"];
          custom.electric-pair-pairs = ''
           '((?\" . ?\")
             (?\[ . ?\])
             (?\( . ?\))
             (?\{ . ?\}))
          '';
          config = ''
            ;; < & > are not delimiters. Change my mind.
            ;; Courtesy of DT. https://gitlab.com/dwt1/configuring-emacs/-/tree/main/07-the-final-touches?ref_type=heads
            (gsetq electric-pair-inhibit-predicate `(lambda (c)
            					   (if (or (char-equal c ?<) (char-equal c ?>))
            					       t
            					       (,electric-pair-inhibit-predicate c))))
          '';
        };
        
        server = {
          enable = true;
          deferIncrementally = true;
          config = "(server-start)";
        };

        no-littering = {
          enable = true;
          demand = true;
          #no-littering doesn't set this by default so we must place
          #auto save files in the same path as it uses for sessions
          custom.auto-save-file-name-transforms = ''`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))'';
        };

        doom-escape = {
          enable = true;
          package = epkgs: epkgs.doom-utils;
          gfhook = ["('doom-escape-hook (list (lambda () (setq efs/vertico-active nil)) 'transient-quit-one))"];
          general."C-g" = "'doom/escape";
          config = ''
            (with-eval-after-load 'eldoc
              (eldoc-add-command 'doom/escape))
          '';
        };

        async = {
          enable = true;
          config = ''
            (autoload 'dired-async-mode "dired-async.el" nil t)
            (dired-async-mode)
          '';
        };

        wgrep = {
          enable = true;
          custom.wgrep-auto-save-buffer = "t";
          generalTwo."'normal".grep-mode-map."w" = "'wgrep-change-to-wgrep-mode";
        };

        ledger = {
          enable = true;
          package = epkgs: epkgs.ledger-mode;
          mode = [''"\\.ledger\\'"''];
        };

        ednc = {
          enable = true;
          gfhook = [
            "('ednc-notification-presentation-functions #'show-notification-in-echo-area)"
          ];
          deferIncrementally = true;
          config = ''
            (ednc-mode)
            (defun show-notification-in-echo-area (old new)
              (when new (message (ednc-format-notification new t))))
          '';
        };
        
      };

      postlude = ''
        ;; Stolen from Derek Taylor's config.
        (add-to-list 'default-frame-alist '(alpha-background . 90))
        ;; Display buffer rules
        (cl-pushnew (list (rx "*Async Shell Command*" (0+ any)) #'display-buffer-no-window) display-buffer-alist)
        (cl-pushnew (list (rx "*Shell Command Output*" (0+ any)) #'display-buffer-no-window) display-buffer-alist)
      '';
    };
  };
}
