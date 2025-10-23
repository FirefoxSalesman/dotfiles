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
        (general-unbind "C-h")
        
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
          :gfhookf ('doom-escape 'hydra-keyboard-quit))
        
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
        
        (defmacro cmd! (&rest body)
          "Returns (lambda () (interactive) ,@body)
        A factory for quickly producing interaction commands, particularly for keybinds
        or aliases. Stolen from Doom."
          (declare (doc-string 1) (pure t) (side-effect-free t))
          `(lambda (&rest _) (interactive) ,@body))
        
        (defmacro local! (var body)
          "Creates a lambda that runs setq-local on the variable VAR with the value provided by BODY."
          `(lambda () (setq-local ,var ,body)))
        
        (defun gen-mode-hooks (modes)
          "Takes a list of symbols, MODES, & appends -mode to them."
          (mapcar (lambda (mode)
        	    (intern (concat (symbol-name mode) "-mode")))
        	  modes))
        
        (defmacro efs/evil-collection-remap (fun state map &rest args)
          "Adds more key definitions directly after running some evil-collection setup function.
        `FUN` is the evil-collection function to advise.
        `STATE` is the evil state to bind the keys in.
        `MAP` is the keymap to bind the keys to.
        `ARGS` is the actual key definitions."
          `(general-add-advice ,fun :after
        		       (lambda () (general-def ,state ,map ,@args))))
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
          setopt = {
            display-line-numbers-type = "'relative";
            display-line-numbers-width = 3;
          }; 
          config = "(global-display-line-numbers-mode)";
          #Disable line numbers for some modes
          ghookf = ["((gen-mode-hooks '(org term dired eww eat markdown help helpful Info Man shell pdf-view elfeed-search elfeed-show eshell racket-repl sage-shell nov)) (lambda () (display-line-numbers-mode 0)))"];
        } ;
        
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
          setopt.auto-save-file-name-transforms = ''`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))'';
        };

        files = {
          enable = true;
          custom.trusted-content = [''"/home/holschcc/projects/RESOLVE/"''];
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
          setopt.wgrep-auto-save-buffer = true;
          generalTwo.":n".grep-mode-map."w" = "'wgrep-change-to-wgrep-mode";
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
