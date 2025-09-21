  { config, inputs, lib, pkgs, ... }:

  {
      stylix = {
        enable = true;
        polarity = "dark";
        targets = {
          vencord.enable = false;
          vesktop.enable = true;
          emacs.enable = false;
        };
        image = ./wallpaper.png;
        cursor = {
          package = pkgs.nordzy-cursor-theme;
          name = "Nordzy-cursors-white";
          size = 16;
        };
        opacity = {
          applications = 0.9;
          popups = 0.9;
          terminal = 0.9;
        };
        fonts = with pkgs; rec {
          monospace = {
            package = jetbrains-mono;
            name = "JetBrains Mono";
          };
          sansSerif = {
            package = inputs.apple-fonts.packages.${system}.sf-pro;
            name = "SF Pro";
          };
          serif = sansSerif;
        };
      };
    
      home.file = {
        ".cache/colors.json".source = config.lib.stylix.colors {
          template = builtins.readFile ./pywal.json.mustache;
          extension = ".json";
        };
      };
    
      home.activation =  #ALSO ACTIVATES AT REBOOT
        {
          generate_pywal_colors = lib.hm.dag.entryAfter ["writeBoundary"] ''
             $DRY_RUN_CMD ${pkgs.pywal}/bin/wal -f ~/.cache/colors.json
             $DRY_RUN_CMD ${pkgs.pywal}/bin/wal -R
           '';
        } ;

      services.picom = {
        enable = true;
        package = (config.lib.nixGL.wrap pkgs.picom);
        backend = "glx";
        opacityRules = [ "70:class_g = 'lmms'" ];
        settings = {
          glx-no-stencil = true;
          glx-no-rebind-pixmap = true;
          use-damage = true;
          vsync = true;
          blur = {
            method = "gaussian";
            size = 5;
            deviation = 2.0;
          };
          shadow-exclude = [ "class_g != 'emacs'"
          ];
        };
        shadow = true;
      };

    programs.emacs.init.usePackage = {
        prettify-symbols = {
          enable = true;
          ghookf = ["('prog-mode 'prettify-symbols-mode)"];
        };

        nerd-icons = {
          enable = true;
          command = [
            "nerd-icons-octicon"
            "nerd-icons-faicon"
            "nerd-icons-flicon"
            "nerd-icons-wicon"
            "nerd-icons-mdicon"
            "nerd-icons-codicon"
            "nerd-icons-devicon"
            "nerd-icons-ipsicon"
            "nerd-icons-pomicon"
            "nerd-icons-powerline"
          ];
        };
      
        all-the-icons = {
          enable = true;
        };
      
        nerd-icons-completion = {
          enable = true;
          ghookf = ["('marginalia-mode 'nerd-icons-completion-marginalia-setup)"];
        };
      
        dashboard = {
          enable = true;
          ghookf = ["('on-init-ui '(dashboard-insert-startupify-lists dashboard-initialize))"];
          config = ''
              (dashboard-setup-startup-hook)
              (dashboard-open)
              (evil-collection-dashboard-setup)
              (evil-collection-dashboard-setup-jump-commands)  
          '' ;
          custom = {
            dashboard-banner-logo-title = ''"Emacs: The one true desktop environment"'';
            dashboard-center-content = true;
            dashboard-items = '''((recents   . 5)
                                  (bookmarks . 5)
                                  (projects  . 5)
                                  (agenda    . 5))'';
            dashboard-icon-type = "'nerd-icons";
            dashboard-set-heading-icons = true;
            dashboard-set-file-icons = true;
            dashboard-agenda-sort-strategy = "'(time-up)";
          };
        };
      
        doom-nano-modeline = {
          enable = true;
          afterCall = ["after-init-hook"];
          custom.mode-line-format = false;
          config = ''
              (defun doom-nano-modeline--render (left right &optional hide-evil-mode)
                "Render the doom-nano modeline string.
            
                LEFT is the information that will be rendered to the left of the modeline. RIGHT
                is the information that will be rendered to the right of modeline. Both
                variables must be a list in which each element has the following syntax:
            
                    (text . face)
            
                where TEXT will be decorated with FACE.
            
                If HIDE-EVIL-MODE is nil, the Evil mode state is not shown in the modeline."
                (let* ((window (get-buffer-window (current-buffer)))
            
                       ;; Variable to store if the this window is active.
              	 (active t)
            
                       ;; Status of the buffer.
                       (status (doom-nano-modeline-status))
            
                       ;; Check if we are recording a macro and get its name.
                       (hasmacro (or defining-kbd-macro executing-kbd-macro))
                       (macroname (if (bound-and-true-p evil-this-macro)
                                      (char-to-string evil-this-macro)
                                    "?"))
            
                       ;; String to indicate the current evil mode.
                       (evilstate
                        (if hide-evil-mode
                            nil
                          (concat (cond ((eq evil-state 'emacs)    "E ")
                                        ((eq evil-state 'motion)   "M ")
                                        ((eq evil-state 'normal)   "N ")
                                        ((eq evil-state 'insert)   "I ")
                                        ((eq evil-state 'replace)  "R ")
                                        ((eq evil-state 'operator) "O ")
                                        ((eq evil-state 'god) "G ")
                                        ((eq evil-state 'symex) "S ")
                                        ((eq evil-state 'visual) (cond ((eq evil-visual-selection 'line)  "L ")
                                                                       ((eq evil-visual-selection 'block) "B ")
                                                                       (t                                 "V ")))
                                        (t "? ")))))
            
                       ;; String to indicate if a macro is being recorded.
                       (macrostring (if hasmacro (concat "● " macroname ) nil))
            
                       ;; Select the modeline face.
              	 (modeline-face 'doom-nano-modeline-active-face)
            
                       ;; Select the face to highlight the evil state.
                       (evilstate-face
                        (cond (hide-evil-mode            modeline-face)
                              ((not active)              modeline-face)
                              ((eq evil-state 'emacs)    'doom-nano-modeline-evil-emacs-state-face)
                              ((or (eq evil-state 'normal) (eq evil-state 'god) (eq evil-state 'symex))   'doom-nano-modeline-evil-normal-state-face)
                              ((eq evil-state 'motion)   'doom-nano-modeline-evil-motion-state-face)
                              ((eq evil-state 'insert)   'doom-nano-modeline-evil-insert-state-face)
                              ((eq evil-state 'replace)  'doom-nano-modeline-evil-replace-state-face)
                              ((eq evil-state 'operator) 'doom-nano-modeline-evil-operator-state-face)
                              ((eq evil-state 'visual)   'doom-nano-modeline-evil-visual-state-face)
                              (t                         modeline-face)))
            
                       ;; Select the face to highlight the macro recording indicator.
                       (macro-face (if hasmacro 'doom-nano-modeline-macro-face modeline-face))
            
                       ;; Assemble the left string with the highlights.
                       (pleft (concat
                               (propertize " "
                                           'face evilstate-face
                                           'display `(raise ,doom-nano-modeline-top-padding))
            
                               ;; Evil state.
                               (when evilstate
                                 (concat (propertize evilstate 'face evilstate-face)
                                         (propertize " " 'face modeline-face)))
            
                               ;; Macro recording indicator.
                               (when macrostring
                                 (concat (propertize macrostring 'face macro-face)
                                         (propertize " " 'face modeline-face)))
            
                               ;; Left list.
                               (if left
                                   (mapconcat
                                    (lambda (element)
                                      (if (and active (cdr element))
                                          (propertize (car element) 'face (cdr element))
                                        (propertize (car element) 'face modeline-face)))
                                    left
                                    "")
                                 ""))))
            
                  ;; Concatenate and return the modeline string.
                  (concat pleft
                          ;; We have one final space as margin, so we make sure it is
                          ;; highlighted with the correct face.
                          (propertize " " 'face modeline-face))))
            
              (defun doom-nano-modeline-org-mode-buffer-name-and-major-mode ()
                "Return the buffer name and the major mode for Org buffers."
                (if (derived-mode-p 'org-mode)
                    (let* ((org-title (doom-nano-modeline--get-org-title))
                           (buffer-name (if org-title
                                            org-title
                                          (format-mode-line "%b")))
                           (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " "")))
            
                      `((,(concat buffer-modified buffer-name) . nil)
              	  ("  " . nil)
                        (,(nerd-icons-icon-for-buffer) . doom-nano-modeline-major-mode-face)
              	  ("  " . nil)))
                  (doom-nano-modeline-default-mode)))
            
              (defun doom-nano-modeline-buffer-name-vc-and-major-mode ()
                "Return the buffer name and the major mode."
                (let* ((buffer-name (cond
                                     ((and (derived-mode-p 'org-mode)
                                           (buffer-narrowed-p)
                                           (buffer-base-buffer))
                                      (format"%s [%s]" (buffer-base-buffer)
                                             (org-link-display-format
                                              (substring-no-properties (or (org-get-heading 'no-tags)
                                                                           "-")))))
                                     ((and (buffer-narrowed-p)
                                           (buffer-base-buffer))
                                      (format"%s [narrow]" (buffer-base-buffer)))
                                     (t
                                      (format-mode-line "%b"))))
            
                       (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " ""))
            
                       (vc-branch-name (doom-nano-modeline--get-vc-branch))
            
                       (vc-branch (if vc-branch-name
                                      `((vc-branch-name . nil))
                                    nil)))
            
                  `((,(concat buffer-modified buffer-name) . nil)
                    ("  " . nil)
                    (,(if vc-branch-name (concat vc-branch-name " ") "") . doom-nano-modeline-vc-branch-name-face)
                    (,(if vc-branch-name " " "") . nil)
                    (,(if (or (equal major-mode 'nix-mode) (equal major-mode 'bibtex-mode)) (all-the-icons-icon-for-buffer) (nerd-icons-icon-for-buffer)) . doom-nano-modeline-major-mode-face)
                    ("  " . nil))))
            
              (defun doom-nano-modeline--special-mode-p ()
                "Return t if we are in `special-mode' or nil otherwise."
                (or (derived-mode-p 'special-mode) (and (eq major-mode 'exwm-mode) (not qutebrowser-exwm-mode))))
            
              (defun doom-nano-tabline ()
                "Format the modeline for the tabline"
                (let* ((the-format '((:eval
              			(funcall
              			 (or (catch 'found
              			       (dolist (elt doom-nano-modeline-mode-formats)
              				 (let* ((config (cdr elt))
              					(mode-p (plist-get config :mode-p))
              					(format (plist-get config :format)))
              				   (when mode-p
              				     (when (funcall mode-p)
              				       (throw 'found format))))))
              			     #'doom-nano-modeline-default-mode-format))))))
                  `((global menu-item ,(format-mode-line the-format) ignore))))
          '';
        };

        ewal = {
          enable = true;
          demand = true;
          custom = {
            ewal-use-built-in-always-p = false;
            ewal-use-built-in-on-failure-p = true;
            ewal-built-in-palette = ''"sexy-material"'';
          };
        };
      
        ewal-doom-themes = {
          enable = true;
          demand = true;
          config = ''
              (load-theme 'ewal-doom-one t)
              ;; Stolen from Noctuid
              (let (custom--inhibit-theme-enable)
                (custom-theme-set-faces
                 'ewal-doom-one
                 `(font-lock-number-face ((t (:foreground ,(ewal-get-color 'blue)))))
                 `(markdown-header-face ((t (:foreground ,(ewal-get-color 'blue)))))
                 `(markdown-header-delimiter-face ((t (:foreground ,(ewal-get-color 'blue)))))
                 `(markdown-bold-face ((t (:foreground ,(ewal-get-color 'green)))))
                 `(markdown-list-face ((t (:foreground ,(ewal-get-color 'green)))))
                 `(org-code ((t (:foreground ,(ewal-get-color 'green)))))
                 `(line-number ((t (:foreground ,(ewal-get-color 'blue)))))
                 `(eshell-git-prompt-powerline-dir-face ((t (:background ,(ewal-get-color 'blue)))))
                 `(hl-line-face ((t (:background ,(ewal-get-color 'green)))))
                 `(solaire-hl-line-face ((t (:background ,(ewal-get-color 'green)))))
                 `(tab-bar ((t :inherit mode-line)))
                 `(eshell-git-prompt-powerline-clean-face ((t (:background ,(ewal-get-color 'green)))))
                 `(eshell-git-prompt-powerline-not-clean-face ((t (:background ,(ewal-get-color 'red)))))))
              (doom-themes-visual-bell-config)
              (doom-themes-org-config)
          '';
          custom = {
            doom-themes-enable-bold = true;
            doom-themes-enable-italic = true;
          };
        };
      
        solaire-mode = {
          enable = true;
          demand = true;
          config = ''(solaire-global-mode)'';
        };
      
      vertico-posframe = {
        enable = true;
        defer = true;
        ghookf = ["('vertico-mode 'vertico-posframe-mode)"];
        config = ''(set-face-attribute 'vertico-posframe-face nil :family 'variable-pitch)'';
      };
    };
  }
