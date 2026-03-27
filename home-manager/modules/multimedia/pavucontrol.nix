{
  flake.homeModules.media = { pkgs, ... }:
  {
    home.packages = [ pkgs.pavucontrol ];
    programs.emacs.init.usePackage.pulseaudio-control = {
      enable = true;
      deferIncrementally = true;
      setopt.pulseaudio-control-volume-step = ''"5%"'';
      general = {
	"<XF86AudioRaiseVolume>" = "'pulseaudio-control-increase-sink-volume";
	"<XF86AudioLowerVolume>" = "'pulseaudio-control-decrease-sink-volume";
	"<XF86AudioMute>" = "'pulseaudio-control-toggle-current-sink-mute";
	"s-v" = "'pulseaudio-control-default-sink-mode";
      };
      config = "(pulseaudio-control-default-sink-mode)";
    };
  };
}
