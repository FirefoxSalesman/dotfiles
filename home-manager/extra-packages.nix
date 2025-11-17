{ config, pkgs, pkgs-stable, ... }:

{
  home.packages = with pkgs; [
    (config.lib.nixGL.wrap gimp)
    (config.lib.nixGL.wrap prismlauncher)
    (config.lib.nixGL.wrap blockbench)
    (config.lib.nixGL.wrap tor-browser)
    wget
    zip
    unzip
    zbar
    # comms
    (config.lib.nixGL.wrap teams-for-linux)
    (config.lib.nixGL.wrap thunderbird)
    pkgs-stable.vesktop
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
}
