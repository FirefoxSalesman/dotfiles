{
  flake.homeModules.exwm =
    {
      config,
      pkgs,
      lib,
      ...
    }:

    {
      programs.emacs.init = {
        tools = {
          goldenRatio = true;
          exwm = {
            enable = true;
            wantMouseWarping = true;
            useGaps = true;
            bindings = {
              "?\\s-r" = "exwm-reset";
              "?\\s-a" = "evil-ex";
              "?\\s-d" = "app-launcher-run-app";
              "?\\s-t" = "proced";

              # Movement
              "?\\s-e" = "pertab-focus-down";
              "?\\s-o" = "pertab-focus-up";
              "?\\s-i" = "pertab-focus-right";
              "?\\s-n" = "pertab-focus-left";
              "?\\s-." = "other-frame";
              "?\\s->" = "tab-bar-move-tab-to-frame";

              # Arrangement
              "?\\s-E" = "pertab-move-down";
              "?\\s-O" = "pertab-move-up";
              "?\\s-I" = "pertab-move-right";
              "?\\s-N" = "pertab-move-left";
              "?\\s-c" = "pertab-horizontal-split";
              "?\\s-C" = "pertab-vertical-split";
              "?\\s-j" = "winner-undo";
              "?\\s-J" = "winner-redo";
              "?\\s-x" = "pertab-layout-menu";
              "?\\s-k" = "pertab-close-window";
              "?\\s-K" = "pertab-kill-buffer-close-window";

              #Shell bindings
              "?\\s-s" = '',(cmd! (shell-command "slock"))'';
              "?\\s-y" =
                '',(cmd! (start-process-shell-command "maim" nil  "${lib.getExe pkgs.maim} ~/pic/screenshot.png"))'';
              "XF86MonBrightnessDown" = ",(cmd! (efs/alter-monitor-brightness 5 t))";
              "XF86MonBrightnessUp" = ",(cmd! (efs/alter-monitor-brightness 5))";
            };
            titleAlterations = {
              qutebrowser = ''(exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))'';
              mpv = ''(exwm-workspace-rename-buffer (format "Mpv: %s" exwm-title))'';
            };
          };
        };
        usePackage.exwm = {
          gfhookf = [
            # When window "class" updates, use it to set the buffer name
            "('exwm-update-class (lambda () (exwm-workspace-rename-buffer exwm-class-name)))"
            # When EXWM starts up, do some extra configuration
            ''
              ('exwm-init (lambda ()
                          (start-process-shell-command "xbanish" nil "${lib.getExe pkgs.xbanish}")))''
          ];
          # Ctrl+q will enable the next key to be sent directly
          generalOneConfig.exwm-mode-map = {
            "C-c" = "mode-specific-map";
          };
          custom = {
            exwm-manage-force-tiling = true;
            # Emacs everywhere
            exwm-input-simulation-keys = '''(([?\C-h] . [backspace]))'';
            exwm-workspace-number = lib.lists.length config.hosts.exwm-monitors;
            # This will need to be updated to the name of a display!  You can find
            # the names of your displays by looking at arandr or the output of xrandr
            exwm-randr-workspace-monitor-plist = config.hosts.exwm-monitors;
            # Window focus should follow the mouse pointer
            mouse-autoselect-window = false;
            focus-follows-mouse = false;
            # These keys should always pass through to Emacs
            exwm-input-prefix-keys = lib.mkForce [
              '''?\M-`''
              '''?\C-^''
              '''?\M-&''
              "?\\s-\\M-'"
              '''?\s-b''
              '''?\M-u''
              '''?\M-:''
              '''?\s-o''
              '''?\s-c''
              '''?\s-v''
              '''?\s-n''
              '''?\s-i''
              '''?\s-e''
              '''?\s-f''
              '''?\s-F''
              '''?\s-u''
              '''?\s-/''
              '''?\s-\H-i''
              '''?\s-\H-n''
              '''?\s-\H-e''
              '''?\s-\H-o''
              "?\\s-'" # popper-toggle-latest
              '''?\s-\"'' # popper-toggle-type
              '''?\s-\ ''
              "'XF86AudioRaiseVolume"
              "'XF86AudioLowerVolume"
              "'XF86AudioMute"
              '''?\M-\ ''
            ];
          };
          init = ''
            (defvar efs/monitor-brightness 27 "The percent brightness of the monitor.")
            
            (defun efs/alter-monitor-brightness (inc &optional neg)
              "Alters the monitor's brightness.
            INC is the percent to increment the volume by.
            NEG subtracts if it is true."
              (setopt efs/monitor-brightness (funcall (if neg '- '+) efs/monitor-brightness inc))
              (shell-command (concat "brightnessctl -d intel_backlight set " (int-to-string efs/monitor-brightness) "%")))
          '';
          config = ''
            (winner-mode)
            
            (repeaters-define-maps
             '(("delete-windows"
                pertab-kill-buffer-close-window "K"
                pertab-close-window "k")))
            (repeaters-define-maps
             '(("input-keys"
                exwm-input-send-next-key "q")))
            (exwm-input-set-key (kbd "s-<return>") 'efs/make-eshell)
          '';
          after = [ "repeaters" ];
        };
      };
    };
}
