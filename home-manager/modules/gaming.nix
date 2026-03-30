{
  flake.homeModules.gaming = { config, pkgs, ... }: {
    home = {
      file.".alsoftrc".text = ''
	[general]
        drivers=pulse
        hrtf=true
      '';
      packages = with pkgs; [
	(config.lib.nixGL.wrap steam)
	(config.lib.nixGL.wrap lutris)
	(config.lib.nixGL.wrap retroarch)
	(config.lib.nixGL.wrap gamemode)
	(config.lib.nixGL.wrap prismlauncher)
      ];
    };
  };
}
