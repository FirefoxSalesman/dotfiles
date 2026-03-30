{ inputs, ... }:

{
  flake.homeModules.aesthetics = { config, lib, pkgs, ... }: {
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

    home.file = {
      ".cache/colors.json".source = config.lib.stylix.colors {
	template = builtins.readFile ./pywal.json.mustache;
	extension = ".json";
      };
    };

    home.activation =  #ALSO ACTIVATES AT REBOOT
      {
	generate_pywal_colors = lib.hm.dag.entryAfter ["writeBoundary"] ''
	  $DRY_RUN_CMD ${lib.getExe pkgs.pywal} -f ~/.cache/colors.json
       $DRY_RUN_CMD ${lib.getExe pkgs.pywal} -R
	'';
      };

    programs.emacs.init.usePackage = {
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
	config = ''(ewal-evil-cursors-get-colors :apply t)'';
      };
    };
  };
}
