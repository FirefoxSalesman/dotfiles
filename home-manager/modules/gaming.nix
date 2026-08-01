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
        # gamemode
        prismlauncher
        graalvmPackages.graalvm-oracle_17
      ];
    };

    programs.lutris = {
      enable = true;
      runners = {
	# libretro.package = pkgs.retroarch;
	ppsspp.package = pkgs.ppsspp;
      };
    };
  };
}
