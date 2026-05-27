{
  flake.homeModules.emacs-host = { ... }:
  {
    hosts = {
      xrandr-command = ''
	xrandr --setprovideroutputsource modesetting NVIDIA-0
        xrandr --auto
        xrandr --dpi 96
      '';
      exwm-monitors = [''0 "eDP-1-1"''];
    };
  };
  flake.homeModules.monitors-host = { ... }:
  {
    hosts = {
      xrandr-command = ''
        xrandr --auto
      '';
      exwm-monitors = [''0 "DP-1"'' ''1 "DVI-I-1"''] ;
    };
  };
}
