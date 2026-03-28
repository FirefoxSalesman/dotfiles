{
  flake.homeModules.web = { pkgs, ... }: {
    programs.emacs.init.usePackage.yeetube = {
      enable = true;
      setopt.yeetube-play-function = "'mpv-play-url";
      generalOne.global-leader."y" = '''("search" . yeetube-search)'';
      generalTwoConfig.":n"."yeetube-mode-map" = {
	"RET" = "'yeetube-play";
	"r" = "'yeetube-channel-videos";
	"s" = "'yeetube-channel-search";
	"d" = "'yeetube-download-video";
      };
    };
  } ;
}
