{
  flake.homeModules.aesthetics = {config, pkgs, ... }: {
    services.picom = {
      enable = true;
      package = (config.lib.nixGL.wrap pkgs.picom);
      backend = "glx";
      opacityRules = [ "70:class_g = 'lmms'" ];
      settings = {
	use-damage = true;
	vsync = true;
	blur = {
	  method = "gaussian";
	  size = 5;
	  deviation = 2.0;
	};
	shadow-exclude = [ "class_g != 'emacs'" ];
      };
      shadow = true;
    };
    programs.emacs.init = {
      usePackage.solaire-mode = {
	enable = true;
	demand = true;
	config = "(solaire-global-mode)";
      };
      # Stolen from Derek Taylor's config.
      postlude = "(add-to-list 'default-frame-alist '(alpha-background . 90))";
    };
  };
}
