{
  flake.homeModules.extraPackages = { config, pkgs, ... }:

  {
    home.packages = with pkgs; [
      (config.lib.nixGL.wrap gimp)
      (config.lib.nixGL.wrap blockbench)
      (config.lib.nixGL.wrap tor-browser)
      wget
      zbar
      # scripts
      pkg
      updatefix
      hdmihelper
    ];
  };
}
