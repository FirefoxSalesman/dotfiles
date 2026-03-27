{
  perSystem = { pkgs, ... }: {
    packages.i3status-rs = pkgs.writeShellScriptBin "i3status-rust" ''
      ${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml
    '';
  };

  flake.homeModules.aesthetics = { pkgs, lib, config, ... }:
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
	    separator_bg = "#" + config.lib.stylix.colors.base00;
	    separator_fg = "#" + config.lib.stylix.colors.base05;
	    separator = "  ";
	  };
	};
      };

      emacs.init.usePackage.i3bar = {
	enable = true;
	ghookf = ["('tab-bar-mode 'i3bar-mode)"];
	setopt.i3bar-command = ''"${lib.getExe pkgs.i3status-rs}"'';
      };
    };
  };
}
