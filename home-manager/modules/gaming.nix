{
  flake.homeModules.gaming = { config, pkgs, ... }: {
    home = {
      file.".alsoftrc".text = ''
	[general]
        drivers=pulse
        hrtf=true
      '';
      packages = with pkgs; [
	# steam
	# lutris
	# retroarch
	# gamemode
	prismlauncher
	graalvmPackages.graalvm-oracle_17
      ];
    };
  };
}
