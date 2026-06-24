{ inputs, ... }:

{
  perSystem = { pkgs, ... }: {
    packages.mpv = (
      inputs.wrapper-modules.wrappers.mpv.wrap {
        pkgs = pkgs;
        script = {
          thumbnail.path = pkgs.mpvScripts.thumbnail;
          sponsorblock.path = pkgs.mpvScripts.sponsorblock;
          quality-menu.path = pkgs.mpvScripts.quality-menu;
        };
        "mpv.conf".content = ''
          fs=no
          hwdec=auto
          x11-bypass-compositor=no
          volume-max=250
        '';
      }
    );
  };

  flake.homeModules.media = { pkgs, config, ... }: {
    home.packages = [ pkgs.yt-dlp ];
    programs = {
      emacs.init.usePackage.mpv = {
        enable = true;
        command = [
          "efs/mpv-browse-url"
          "efs/mpv-eww-url"
        ];
        config = ''
          (defun efs/mpv-browse-url (url &optional single)
            (mpv-play-url url))
          
          (defun efs/mpv-eww-url ()
            (interactive)
            (mpv-play-url (eww-current-url)))
        '';
      };

      mpv.enable = true;
    };
  };
}
