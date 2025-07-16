{ config, pkgs, pkgs-stable, ... }:

{
  home.packages = with pkgs; [
    (config.lib.nixGL.wrap pkgs-stable.gimp)
    (config.lib.nixGL.wrap prismlauncher)
    (config.lib.nixGL.wrap blockbench)
    wget
    zip
    unzip
    zbar
    # comms
    (config.lib.nixGL.wrap teams-for-linux)
    (config.lib.nixGL.wrap thunderbird)
    vesktop
    # things emacs appreciates
    xclip
    xsel
    xdotool
    # scripts
    ezf
    pkg
  ];
}
