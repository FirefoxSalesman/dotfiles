{ inputs, ... }:

{
  flake.homeModules.aesthetics =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      wayland.windowManager.hyprland.configType = "lua";

      stylix = {
        enable = true;
        polarity = "dark";
        targets.emacs.enable = false;
        image = ../../wallpaper.png;
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
            package = pkgs.jetbrains-mono;
            name = "JetBrains Mono";
          };
          sansSerif = {
            package = inputs.apple-fonts.packages.${system}.sf-pro;
            name = "SF Pro";
          };
          serif = sansSerif;
        };
      };

      home = {
        file = {
          ".cache/colors.json".source = config.lib.stylix.colors {
            template = builtins.readFile ./pywal.json.mustache;
            extension = ".json";
          };
        };

        activation = # ALSO ACTIVATES AT REBOOT
          {
            generate_pywal_colors = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              $DRY_RUN_CMD ${lib.getExe pkgs.pywal} -f ~/.cache/colors.json
              $DRY_RUN_CMD ${lib.getExe pkgs.pywal} -R
            '';
          };

        pointerCursor.enable = true;
      };

      programs.emacs.init.usePackage = {
        ewal-doom-themes = {
          enable = true;
          demand = true;
          config = ''
            	  (setcdr
            	   (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
            	   '(:inherit 'gnus-group-mail-1-empty :weight 'normal))
            	  (load-theme 'ewal-doom-one t)
            	  ;; Stolen from Noctuid
            	  (let (custom--inhibit-theme-enable)
            	    (let* ((blue (ewal-get-color 'blue))
            	           (green (ewal-get-color 'green))
            	           (red (ewal-get-color 'red)))
            	      (custom-theme-set-faces
            	       'ewal-doom-one
            	       `(font-lock-number-face ((t (:foreground ,blue))))
            	       `(markdown-header-face ((t (:foreground ,blue))))
            	       `(markdown-header-delimiter-face ((t (:foreground ,blue))))
            	       `(markdown-bold-face ((t (:foreground ,green))))
            	       `(markdown-list-face ((t (:foreground ,green))))
            	       `(org-code ((t (:foreground ,green))))
            	       `(line-number ((t (:foreground ,blue))))
            	       `(eshell-git-prompt-powerline-dir-face ((t (:background ,blue))))
            	       `(hl-line-face ((t (:background ,green))))
            	       `(solaire-hl-line-face ((t (:background ,green))))
            	       `(tab-bar ((t :inherit mode-line)))
            	       `(eshell-git-prompt-powerline-clean-face
            	         ((t (:background ,green))))
            	       `(eshell-git-prompt-powerline-not-clean-face
            	         ((t (:background ,red)))))))
            	  (doom-themes-visual-bell-config)
            	  (doom-themes-org-config)
            	'';
          setopt = {
            ewal-use-built-in-always-p = false;
            ewal-use-built-in-on-failure-p = true;
            ewal-built-in-palette = ''"sexy-material"'';
            doom-themes-enable-bold = true;
            doom-themes-enable-italic = true;
          };
        };

        ewal-evil-cursors = {
          enable = true;
          demand = true;
          config = "(ewal-evil-cursors-get-colors :apply t)";
        };
      };
    };
}
