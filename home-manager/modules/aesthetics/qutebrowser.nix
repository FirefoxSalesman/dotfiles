{
  flake.homeModules.web = { lib, config, ... }: {
    programs.qutebrowser.settings = {
      window.transparent = true;
      colors = {
	completion = {
	  category = {
	    bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
	    border = {
              bottom = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
              top = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
	    };
	  };
	  even.bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
	  odd.bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
	};
	statusbar = {
	  command = {
	    bg = lib.mkForce ("#50" + config.lib.stylix.colors.base00);
	    private.bg = lib.mkForce ("#50" + config.lib.stylix.colors.base00);
	  };
	};
	webpage.darkmode = {
	  enabled = true;
	  algorithm = "lightness-cielab";
	  policy.images = "never";
	};
      };
    };
  };
}
