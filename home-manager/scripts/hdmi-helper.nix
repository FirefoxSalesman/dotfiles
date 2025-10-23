{ pkgs }:

pkgs.writeShellScriptBin "hdmihelper" ''
  case "$1" in
    mirror ) xrandr --output HDMI-1-0 --mode 1920x1080 --output eDP-1 --mode 1920x1080 ;;
    off ) xrandr --output eDP-1 --mode 2560x1600 ;;
  esac
''
