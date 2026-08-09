{
  flake.homeModules = {
    emacs-host.hosts = {
      xrandr-command = ''
        	xrandr --setprovideroutputsource modesetting NVIDIA-0
          xrandr --auto
          xrandr --dpi 96
      '';
      exwm-monitors = [ ''0 "eDP-1-1"'' ];
      wantBattery = true;
    };
    monitors-host.hosts = {
      xrandr-command = ''
        	  xrandr --output DP-4 --primary --mode 1920x1080 --output DVI-I-1 --mode 1920x1200
      '';
      exwm-monitors = [
        ''0 "DP-4"''
        ''1 "DVI-I-1"''
      ];
      wantBattery = false;
    };
  };
}
