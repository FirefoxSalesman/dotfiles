{
  perSystem = { lib, pkgs, ... }: {
    packages.startEwm = pkgs.writeShellScriptBin "startewm" ''
      emacs --fg-daemon --eval "(require 'ewm)" --eval "(ewm-start-module)" --eval "(efs/run-in-background \"${lib.getExe pkgs.wbg} ~/.config/home-manager/wallpaper.png\")"
    '';
  };
  flake.homeModules.ewm = { lib, pkgs, ... }: {
    programs.emacs.init.usePackage = {
      dashboard.ghookf = [ "('after-init '(dashboard-insert-startupify-lists dashboard-initialize))" ];
      evil.gfhookf = [ "('after-init 'evil-mode)" ];
      ewm = {
        enable = true;
        defer = true;
        custom.ewm-output-config = [
          [
            ''"DP-1"''
            "':width"
            1920
            "':height"
            1080
          ]
          [
            ''"DVI-I-1"''
            "':width"
            1920
            "':height"
            1200
          ]
        ];
        generalOneConfig.ewm-mode-map = {
          "s-<return>" = "'efs/make-eshell";
          "s-a" = "'evil-ex";
          "s-f" = "'bufler-workspace-focus-buffer";
          "s-." = "'other-frame";
          "s-j" = "'winner-undo";
          "s-J" = "'winner-undo";
        };
        config = ''
          (winner-mode)
          (defun efs/run-in-background (command)
               (let ((command-parts (split-string command "[ ]+")))
                    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))
        '';
        setopt.ewm-intercept-prefixes = [
          ''"s-<return>"''
          ''"s-a"''
          ''"s-b"''
          ''"s-d"''
          ''"s-u"''
          ''"C-^"''
          ''"M-&"''
          ''"s-M-'"''
          ''"s-J"''
          ''"s-j"''
          ''"s-f"''
          ''"s-e"''
          ''"s-o"''
          ''"s-i"''
          ''"s-n"''
          ''"s-."''
          ''"s->"''
          ''"s-E"''
          ''"s-O"''
          ''"s-I"''
          ''"s-N"''
          ''"s-c"''
          ''"s-C"''
          ''"s-x"''
          ''"s-k"''
          ''"s-K"''
        ];
        package = epkgs: pkgs.ewm;
        extraPackages = with pkgs; [
          startEwm
          xwayland-satellite
          wl-clipboard
        ];
      };
    };
  };
}
