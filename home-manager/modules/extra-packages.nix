{ inputs, ... }:
{
  flake.homeModules.extraPackages =
    { config, pkgs, ... }:

    {
      home.packages = with pkgs; [
        gimp
        blockbench
        tor-browser
        wget
        zbar
        # scripts
        ezf
        hdmihelper
        pkg
      ];
    };
}
