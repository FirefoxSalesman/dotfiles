{
  flake.homeModules.extraPackages = { config, pkgs, ... }:

  {
    home.packages = with pkgs; [
      (config.lib.nixGL.wrap gimp)
      (config.lib.nixGL.wrap blockbench)
      (config.lib.nixGL.wrap tor-browser)
      wget
      zip
      unzip
      zbar
      (config.lib.nixGL.wrap steam)
      (config.lib.nixGL.wrap lutris)
      (config.lib.nixGL.wrap retroarch)
      (config.lib.nixGL.wrap gamemode)
      (config.lib.nixGL.wrap prismlauncher)
      # things emacs appreciates
      xclip
      xsel
      xdotool
      # scripts
      ezf
      pkg
      updatefix
      hdmihelper
    ];
  };
}
