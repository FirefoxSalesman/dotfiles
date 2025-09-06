  { inputs, pkgs, pkgs-stable, config, ... }:

  {
    programs = {
        i3status-rust = {
          enable = true;
          bars."default" = {
            blocks = [
        		  {
        			  block = "time";
        			  format =  "$icon $timestamp.datetime(f:'%a %d/%m %R')";
        			  interval = 60;
        		  }
              # {
              #   block = "music";
              # }
              {
        			  block = "battery";
        			  format =  "$icon $percentage";
        			  interval = 60;
        		  }
        	  ];
            icons = "awesome6";
            settings.theme.overrides = {
              idle_bg = "#" + config.lib.stylix.colors.base00;
              idle_fg = "#" + config.lib.stylix.colors.base05;
              info_bg = "#" + config.lib.stylix.colors.base00;
              info_fg = "#" + config.lib.stylix.colors.base05;
              good_bg = "#" + config.lib.stylix.colors.base00;
              good_fg = "#" + config.lib.stylix.colors.base05;
              warning_bg = "#" + config.lib.stylix.colors.base00;
              warning_fg = "#" + config.lib.stylix.colors.base05;
              critical_bg = "#" + config.lib.stylix.colors.base00;
              critical_fg = "#" + config.lib.stylix.colors.base05;
              # alternating_tint_bg = "#" + config.lib.stylix.colors.base00;
              separator_bg = "#" + config.lib.stylix.colors.base00;
              separator_fg = "#" + config.lib.stylix.colors.base05;
              separator = "  ";
            };
          };
        };
      
        emacs.init.usePackage.i3bar = {
          enable = true;
          ghook = ["('tab-bar-mode-hook 'i3bar-mode)"];
          
          custom = {
            i3bar-command = ''"${(import ../scripts/i3status-rust.nix {inherit pkgs;})}/bin/i3status-rust"'';
          };
        };

      emacs.init.usePackage = {
        tab-bar = {
          enable = true;
          config = ''
            (general-add-advice 'tab-new :after #'dashboard-open)
            (defun efs/tab-bar-select ()
               (interactive)
               (setq tab-bar-tab-hints t)
               (tab-bar-select-tab (string-to-number (read-string "Tab Number: ")))
               (setq tab-bar-tab-hints nil))
          '';
          ghook = ["('exwm-init-hook 'tab-bar-mode)"];
          general."s-u" = "'tab-bar-hydra/body";
          custom = {
            tab-bar-format = "'(tab-bar-format-tabs-groups tab-bar-separator doom-nano-tabline tab-bar-format-align-right tab-bar-format-global)";
            tab-bar-close-button-show = false;
            tab-bar-select-restore-windows = false;
            tab-bar-auto-width-max = "'((150) 20)";
          };
          extraConfig = ''
            :pretty-hydra
            ((:color amaranth)
             ("Navigation"
              (("e" #'evil-tab-next "next")
               ("o" #'tab-bar-switch-to-prev-tab "prev")
               ("v" #'tab-recent "recent")
               ("b" #'tab-bar-lost-commands-switch-to-first-tab "first")
               ("B" #'tab-bar-lost-commands-switch-to-last-tab "last")
               ("/" #'efs/tab-bar-select "search"))
              "Creation/Deletion"
              (("s" #'tab-new "new")
               ("k" #'tab-close "close")
               ("r" #'tab-rename "rename")
               ("u" #'tab-undo "undo"))
              "Groups"
              (("g" #'tab-group "add to group")
               ("K" #'tab-close-group "close group"))
              "Organization"
              (("E" #'tab-bar-lost-commands-move-tab-forward "forward")
               ("O" #'tab-bar-lost-commands-move-tab-backward "backward"))
              "Exit"
              (("<return>" nil "" :color blue)
               ("<escape>" nil "" :color blue))))
          '';
        };
        
        tab-bar-lost-commands = {
          enable = true;
          command = [
            "tab-bar-lost-commands-move-tab-forward"
            "tab-bar-lost-commands-move-tab-backward" 
            "tab-bar-lost-commands-switch-to-first-tab" 
            "tab-bar-lost-commands-switch-to-last-tab" 
          ];
        };
        
        bufler = {
          enable = true;
          ghook = ["('tab-bar-mode-hook 'bufler-mode)"];
          general = {
            "s-b" = "'bufler-hydra/body";
            "s-f" = "'bufler-workspace-focus-buffer";
            "s-F" = "'bufler-workspace-set";
          };
          generalTwo."'normal".evil-collection-unimpaired-mode-map = {
            "]b" = "'bufler-cycle-buffers-forward";
            "[b" = "'bufler-cycle-buffers-backward";
          };
          custom.bufler-groups = ''
            (bufler-defgroups
              ;; Subgroup collecting all named workspaces.
              (group (auto-workspace))
              ;; Subgroup collecting buffers in a project.
              (group (auto-project))
              ;; Subgroup collecting tramp buffers
              (group (auto-tramp))
              ;; Grouping browser windows
              (group
               (group-or "Browsers"
                         (name-match "Qutebrowser" (rx bos "Qutebrowser"))
                         (name-match "Tor Browser" (rx bos "Tor Browser"))
                         (mode-match "eww-mode" (rx bos "eww-"))))
              (group
               (group-or "Chat"
                         (name-match "Thunderbird" (rx bos "Thunderbird"))
                         (name-match "teams-for-linux" (rx bos "teams-for-linux"))
                         (mode-match "ement" (rx bos "ement-"))
                         (name-match "vesktop" (rx bos "vesktop"))))
              (group
               (group-or "Media"
                         (name-match "mpv" (rx bos "Mpv"))
                         (mode-match "elfeed-search-mode" (rx bos "elfeed-"))
                         (mode-match "elfeed-show-mode" (rx bos "elfeed-"))
            	     (mode-match "yeetube-mode" (rx bos "yeetube-"))))
              (group
               (group-or "Agenda"
                         (name-match "tasks.org" (rx bos "tasks.org"))
                         (mode-match "org-agenda-mode" (rx bos "org-agenda-"))))
              (group
               (group-or "Ledger"
            	     (mode-match "ledger-mode" (rx bos "ledger-"))
            	     (mode-match "ledger-report-mode" (rx bos "ledger-"))))
              (group
               (group-or "Notes"
                         (dir "~/doc/denote/")))
              (group
               (group-or "AV"
                         (name-match "lmms" (rx bos "lmms"))
                         (name-match "Gimp-2.10" (rx bos "Gimp-2.10"))
                         (name-match "kdenlive" (rx bos "kdenlive"))))
              (group
               (group-or "Games"
                         (dir "~/.local/PrismLauncher/")
                         (name-match "Minecraft" (rx bos "Minecraft"))
                         (name-match "PrismLauncher" (rx bos "PrismLauncher"))))
              (group
               ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
               (group-or "Help/Info"
                         (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                         (mode-match "*Info*" (rx bos "info-"))
            	     (mode-match "Man-mode" (rx bos "Man-"))))
              (group
               ;; Subgroup collecting all special buffers (i.e. ones that are not
               ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
               ;; through to other groups, so they end up grouped with their project buffers).
               (group-and "*Special*"
                          (name-match "**Special**"
                                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                          (lambda (buffer)
                            (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                                 buffer)
                                        (funcall (mode-match "Dired" (rx bos "dired"))
                                                 buffer)
                                        (funcall (auto-file) buffer))
                              "*Special*"))))
              (auto-directory))
          '' ;
          init = ''
              ;; These functions were adapted from perspective-exwm
              (defun bufler-cycle-buffers (proc)
                "Switches to the next or previous buffer in the workspace, if one exists, or the next buffer anywhere, if one doesn't exist."
                (let* ((workspace (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find))))
                  (if workspace
                      (let* ((current (current-buffer))
                             (buffer-list (mapcar #'cdr
                                                  (bufler-buffer-alist-at workspace :filter-fns bufler-filter-buffer-fns)))
                             (current-pos (or (cl-position current buffer-list) -1))
                             (len (length buffer-list))
                             (next-pos (% (+ current-pos len
                                             (if (eq proc 'evil-next-buffer) (- len 1) -1))
                                          len))
                             (next-buffer (nth next-pos buffer-list)))
                        (switch-to-buffer next-buffer))
                    (funcall proc))))
              (defun bufler-cycle-buffers-forward ()
                "Cycles the buffers in the workspace forward."
                (interactive)
                (bufler-cycle-buffers 'evil-next-buffer))
              (defun bufler-cycle-buffers-backward ()
                "Cycles the buffers in the workspace backward."
                (interactive)
                (bufler-cycle-buffers 'evil-prev-buffer))
            
          '';
          config = ''
            (defun bufler-bar ()
              (interactive)
              (bufler-sidebar)
              (with-selected-window (get-buffer-window "*Bufler*")
                (gsetq window-size-fixed 'width)
                (window-resize (selected-window) (- 35 (window-total-width)) t t)))
          '';
          extraConfig = ''
            :pretty-hydra
            ((:color amaranth)
              ("Move"
               (("o" bufler-cycle-buffers-backward "prev")
                ("e" bufler-cycle-buffers-forward "next"))
               "Tricks"
               (("/" consult-buffer "search" :color blue :exit t)
                ("i" ibuffer "list (ibuffer)" :color blue :exit t)
                ("k" kill-current-buffer "delete"))
               "Quit"
               (("<escape>" nil "quit" :color blue :exit t)
                ("<return>" nil "quit" :color blue :exit t))))
          '';
        };
        
        bufler-workspace-tabs = {
          enable = true;
          ghook = ["('bufler-mode-hook 'bufler-workspace-workspaces-as-tabs-mode)"];
          gfhook = ["('bufler-workspace-workspaces-as-tabs-mode-hook '(global-tab-line-mode burly-tabs-mode))"];
        };

          elwm = {
            enable = true;
            defer = true;
            command = ["elwm-next" "elwm-prev" "elwm-rotate-window" "elwm-derotate-window" "elwm-split-window"];
            config = ''
              (defun elwm-split-window ()
                "Split window according to the current layout.
                
                Window in the master area can't be split, instead the last window
                in the stack will be split.
                
                If selected window is window on the stack, the new window will be
                created next to it, according to the current layout."
                (interactive)
                (let ((buf (current-buffer)))
                  (unless (or (eq elwm-current-layout 'monocle) (eq elwm-current-layout 'follow))
              	(if (eq (count-windows) 1)
              	    (if (eq elwm-current-layout 'tile-vertical-left)
              		(evil-window-vsplit)
              	      (evil-window-split))
              	  (cond
              	   ((eq elwm-current-layout 'tile-vertical-left)
              	    (if (elwm--in-master-area-p)
              		;; split the last window on the stack instead
              		(set-window-buffer
              		 (select-window (split-window (car (last (elwm--sorted-window-list))) nil nil))
              		 buf)
              	      (evil-window-split)))
              	   ((eq elwm-current-layout 'tile-horizontal-top)
              	    (if (elwm--in-master-area-p)
              		;; split the last window on the stack instead
              		(set-window-buffer
              		 (select-window (split-window (car (last (elwm--sorted-window-list))) nil t))
              		 buf)
              	      (evil-window-vsplit))))))))
            '';
            init = ''
                (defun elwm-next ()
                  "Go to the next visible window, or if there is 1 window, the next buffer in the workspace"
                  (interactive)
                  (if (eq (count-windows) 1)
                      (bufler-cycle-buffers-forward)
                    (elwm-activate-window)))
                (defun elwm-deactivate-window () (interactive) (elwm-activate-window (prefix-numeric-value -1)))
                (defun elwm-prev ()
                  "Go to the previous visible window, or if there is 1 window, the previous buffer in the workspace"
                  (interactive)
                  (if (eq (count-windows) 1)
                      (bufler-cycle-buffers-backward)
                    (elwm-deactivate-window)))
                (defun elwm-derotate-window () (interactive) (elwm-rotate-window (prefix-numeric-value -1)))
              
            '';
          };

        golden-ratio = {
          enable = true;
          defer = true;
          ghook = ["('on-first-input-hook 'golden-ratio-mode)"];
          config = "(general-add-advice 'golden-ratio :after 'exwm-mff-warp-to-selected)";
        };
        
          exwm = {
            enable = true;
            gfhook = [
              # When window "class" updates, use it to set the buffer name
              "('exwm-update-class-hook 'efs/exwm-update-class)"
              # When EXWM starts up, do some extra configuration
              "('exwm-init-hook 'efs/exwm-init-hook)"
              "('exwm-mode-hook 'evil-motion-state)"
              # When window title updates, use it to set the buffer name
              "('exwm-update-title-hook 'efs/exwm-update-title)"
            ];
            # Ctrl+q will enable the next key to be sent directly
            generalOne.exwm-mode-map."C-q" = "'exwm-input-send-next-key";
            generalTwo.local-leader.exwm-mode-map = {
              "n" = '''(lambda () (interactive) (start-process-shell-command "warpd" nil "warpd --normal"))'';
              "x" = '''(lambda () (interactive) (start-process-shell-command "warpd" nil "warpd --hint"))'';
              "g" = '''(lambda () (interactive) (start-process-shell-command "warpd" nil "warpd --grid"))'';
            };
            custom = {
              exwm-manage-force-tiling = true;
              # Emacs everywhere
              exwm-input-simulation-keys = '''(([?\C-h] . [backspace]))'';
              exwm-workspace-number = "1";
              # Automatically move EXWM buffer to current workspace when selected
              exwm-layout-show-all-buffers = true;
              # Display all EXWM buffers in every workspace buffer list
              exwm-workspace-show-all-buffers = true;
              # This will need to be updated to the name of a display!  You can find
              # the names of your displays by looking at arandr or the output of xrandr
              exwm-randr-workspace-monitor-plist = '''(1 "eDP-1-1") (2 "HDMI-0")'';
              # Automatically send the mouse cursor to the selected workspace's display
              exwm-workspace-warp-cursor = true;
              # Window focus should follow the mouse pointer
              mouse-autoselect-window = false;
              focus-follows-mouse = false;
              # These keys should always pass through to Emacs
              exwm-input-prefix-keys = '''(?\M-`
                                           ?\C-^
                                           ?\M-&
                                           ?\s-\M-'
                                           ?\s-b
                                           ?\M-u
                                           ?\M-:
                                           ?\s-o
                                           ?\s-c
                                           ?\s-v
                                           ?\s-n
                                           ?\s-i
                                           ?\s-e
                                           ?\s-f
                                           ?\s-F
                                           ?\s-u
                                           ?\s-/
                                           ?\s-'	;; popper-toggle-latest
                                           ?\s-\" ;; popper-toggle-type
                                           ?\s-\ 
                                           XF86AudioRaiseVolume
                                           XF86AudioLowerVolume
                                           XF86AudioMute
                                           ?\M-\ )'';
              # Set up global key bindings.  These always work, no matter the input state!
              # Keep in mind that changing this list after EXWM initializes has no effect.
              exwm-input-global-keys = ''`(;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
                                             ([?\s-r] . exwm-reset)
                                             ([?\s-a] . evil-ex)
              
                                              ;; Launch applications (basically dmenu)
                                              ([?\s-d] . app-launcher-run-app)
                                              ([?\s-t] . proced)
              
                                              ;;Movement
                                              ([?\s-e] . elwm-next)
                                              ([?\s-o] . elwm-prev)
              
                                              ;; Arrangement
                                              ([?\s-E] . elwm-rotate-window)
                                              ([?\s-O] . elwm-derotate-window)
                                              ([?\s-c] . elwm-split-window)
                                              ([?\s-n] . evil-window-move-far-left)
                                              ([?\s-i] . evil-window-move-far-right)
                                              ([?\s-j] . winner-undo)
                                              ([?\s-J] . winner-redo)
                                              ([?\s-r] . exwm-reset)
                                              ([?\s-m] . toggle-single-window)
                                              ([?\s-x] . toggle-follow-mode)
                                              ([?\s-k] . evil-window-delete)
                                              ([?\s-K] . evil-delete-buffer-and-window)
              
                                              ;; Shell bindings
                                              ([?\s-s] . (lambda () (interactive) (shell-command "slock")))
                                              ([?\s-y] . (lambda () (interactive) (start-process-shell-command "maim" nil  "${pkgs-stable.maim}/bin/maim ~/pic/screenshot.png"))))'';
              
            };
            afterCall = ["on-init-ui-hook"];
            init = ''
              (defun efs/exwm-init-hook ()
                ;; Make workspace 0 be the one where we land at startup
                (exwm-workspace-switch-create 0)
                
                ;; Show status in the mode line
                (start-process-shell-command "xbanish" nil "${pkgs.xbanish}/bin/xbanish"))
              (defun efs/exwm-update-class ()
                (exwm-workspace-rename-buffer exwm-class-name))
              
              (defun efs/exwm-update-title ()
                (pcase exwm-class-name
                  ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
                  ("mpv" (exwm-workspace-rename-buffer (format "Mpv: %s" exwm-title)))))
              
              ;; From dmacs
              (defvar single-window--last-configuration nil "Last window configuration before calling `delete-other-windows'.")
              ;; From dmacs
              (defun toggle-single-window ()
                "Un-maximize current window.
                If multiple windows are active, save window configuration and
                delete other windows.  If only one window is active and a window
                configuration was previously save, restore that configuration."
                (interactive)
                (if (= (count-windows) 1)
                    (when single-window--last-configuration
              	(setq elwm-current-layout 'tile-vertical-left)
                      (set-window-configuration single-window--last-configuration)
              	(when treesitter-context-mode (treesitter-context-focus-mode -1)))
                  (setq single-window--last-configuration (current-window-configuration))
                  (setq elwm-current-layout 'monocle)
                  (delete-other-windows)
                  (when (and treesitter-context-mode)
                    (treesitter-context-focus-mode 1))))
              
              (defun evil-delete-buffer-and-window ()
                "kill the current buffer & its window"
                (interactive)
                (kill-current-buffer)
                (evil-window-delete))
              
              (defun toggle-follow-mode ()
                "If called while multiple windows are present, deactivates follow mode & kills all other windows.
                 If called on only 1 window, activates follow mode & splits the window."
                (interactive)
                (if (= (count-windows) 1)
                    (progn (follow-mode 1)
              	     (split-window-right)
              	     (setq elwm-current-layout 'follow))
                  (progn (follow-mode -1)
              	   (delete-other-windows)
              	   (setq elwm-current-layout 'tile-vertical-left))))
            '';
            config = ''
                ;; Set the screen resolution (update this to be the correct resolution for your screen!)
                (winner-mode)
                (require 'exwm-randr)
                (exwm-randr-mode)
              
                (repeaters-define-maps
                 '(("delete-windows"
                    evil-delete-buffer-and-window "K"
                    evil-window-delete "k")))
                (repeaters-define-maps
                 '(("input-keys"
                    exwm-input-send-next-key "q")))
                (exwm-input-set-key (kbd "s-<return>") 'efs/make-eshell)
                (exwm-wm-mode)
            '';
            after = ["repeaters"];
          };
        
          exwm-mff = {
            enable = true;
            defer = true;
            command = ["exwm-mff-warp-to-selected"];
          };
        
          exwm-outer-gaps = {
            enable = true;
            config = ''(ignore-errors (exwm-outer-gaps-mode))'';
            after = ["exwm"];
          };
        
          popper = {
            enable = true;
            ghook = ["('on-first-buffer-hook 'popper-mode)"];
            general = {
              "s-'" = "'popper-toggle";
              "s-\\\"" = "'popper-cycle";
              "C-s-'" = "'popper-toggle-type";
            };
            custom = {
              popper-window-height = "30";
              popper-group-function = "'popper-group-by-project";
              popper-reference-buffers = '''(help-mode
                                               helpful-mode
                                               compilation-mode
                                               inferior-python-mode
                                               occur-mode
                                               grep-mode
                                               "^\\*.*eshell\\*"
                                               "^\\*eat\\*"
                                               "^\\*Sage\\*"
                                               "^\\*prolog\\*"
                                               flymake-diagnostics-buffer-mode
                                               rustic-cargo-test-mode
                                               rustic-cargo-run-mode
            			                             geiser-repl-mode
                                               dape-repl-mode
            			                             racket-repl-mode
                                               inferior-ess-r-mode
                                               cider-repl-mode)'';
            };
          };
        
          proced = {
            enable = true;
            command = ["proced"];
            generalTwoConfig.":n".proced-mode-map = {
              "j" = "'proced-unmark";
              "k" = "'proced-send-signal";
            };
          };
        
        ace-window = {
          enable = true;
          custom = {
            aw-scope = "'frame";
            aw-keys = "'(?c ?r ?s ?t ?n ?e ?i ?a)";
          };
          preface = ''
            (defun efs-aw-split-window-elwm (window)
              "Split WINDOW horizontally."
              (select-window window)
              (elwm-split-window))
          '';
          init = ''(setopt aw-dispatch-alist '((?k aw-delete-window "Delete Window")
                                               (?m aw-swap-window "Swap Windows")
                                               (?M aw-move-window "Move Window")
                                               (?d aw-copy-window "Copy Window")
                                               (?b aw-switch-buffer-in-window "Select Buffer")
                                               (?f aw-flip-window)
                                               (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
                                               (?x aw-execute-command-other-window "Execute Command Other Window")
                                               (?v efs-aw-split-window-elwm "Split Window")
                                               (?o delete-other-windows "Delete Other Windows")
                                               (?T aw-transpose-frame "Transpose Frame")
                                               (?? aw-show-dispatch-help)))'';
          general."s-/" = "'ace-window";
          config = ''
            (ace-window-posframe-mode)
            
            (defun aw--lead-overlay-posframe (path leaf)
              (let* ((wnd (cdr leaf))
                     (str (format "%s" (apply #'string path)))
                     ;; It's important that buffer names are not unique across
                     ;; multiple invocations: posframe becomes very slow when
                     ;; creating new frames, and so being able to reuse old ones
                     ;; makes a huge difference. What defines "able to reuse" is
                     ;; something like: a frame exists which hasn't been deleted
                     ;; (with posframe-delete) and has the same configuration as
                     ;; the requested new frame.
                     (bufname (format " *aw-posframe-buffer-%s*" path)))
                (with-selected-window wnd
                  (push bufname aw--posframe-frames)
                  (posframe-show bufname
                                 :string str
                                 :poshandler aw-posframe-position-handler
            		     :refposhandler 'posframe-refposhandler-xwininfo
            		     :parent-frame nil
                                 :font (face-font 'aw-leading-char-face)
                                 :foreground-color (face-foreground 'aw-leading-char-face nil t)
                                 :background-color (face-background 'aw-leading-char-face nil t)))))
            
            (general-add-advice 'ace-window :after (lambda (&rest args) (golden-ratio)))
          '';
        };
      };
    };
  }
