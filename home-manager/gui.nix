{ pkgs, ... }:

{
  home.file.".config/X11/xinitrc".text = '' 
    #!/usr/bin/sh
    xrandr --setprovideroutputsource modesetting NVIDIA-0
    xrandr --auto
    xrandr --dpi 96
    # xrandr --output eDP-1 --mode 2560x1600
    ${pkgs.xwallpaper}/bin/xwallpaper --stretch ~/.config/home-manager/wallpaper.png 
    xrdb load ~/.cache/wal/colors.Xresources 
    
    if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
        eval $(dbus-launch --exit-with-session --sh-syntax)
    fi
    
    xmodmap ~/.Xmodmap
    
    picom &
    
    if command -v dbus-update-activation-environment >/dev/null 2>&1; then
        dbus-update-activation-environment DISPLAY XAUTHORITY
    fi
    
    # exec dbus-launch --exit-with-session emacsclient -c
    exec dbus-launch --exit-with-session emacs -mm --debug-init
  '' ;
}
