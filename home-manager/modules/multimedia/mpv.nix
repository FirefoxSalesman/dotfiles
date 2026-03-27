{
  perSystem = { pkgs, ... }: {
    packages.mpv = (pkgs.mpv.override {
      scripts = with pkgs.mpvScripts; [
	thumbfast
	sponsorblock
	quality-menu
      ];
    });
  };
  
  flake.homeModules.media = { pkgs, config, ... }: {
    home.packages = [ pkgs.yt-dlp ];
    programs = {
      emacs.init.usePackage.mpv = {
	enable = true;
	command = ["efs/mpv-browse-url" "efs/mpv-eww-url"];
	config = ''
	  (defun efs/mpv-browse-url (url &optional single)
	    (mpv-play-url url))
	  
	  (defun efs/mpv-eww-url ()
	    (interactive)
	    (mpv-play-url (eww-current-url)))
	'';
      };

      mpv = {
	enable = true;
	package = (config.lib.nixGL.wrap pkgs.mpv);
	config = {
	  fs = "no";
	  hwdec = "auto";
	  x11-bypass-compositor = "no";
	  volume-max = "250";
	};
      };
    };
  } ;
}
