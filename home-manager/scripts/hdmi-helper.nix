{ pkgs }:

pkgs.writeShellScriptBin "hdmihelper" ''
  case "$1" in
    dual ) xrandr --output HDMI-1-0 --right-of eDP-1 --mode 1920x1080 --output eDP-1 --primary --mode 1920x1080 ;;
    mirror ) xrandr --output HDMI-1-0 --mode 1920x1080 --output eDP-1 --mode 1920x1080 ;;
    off ) xrandr --output eDP-1 --mode 2560x1600 ;;
  esac
''
