{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.mpc-wrapper = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "mpc-wrapper";
	version = "current";
	src = inputs.mpc-wrapper;
      }
    );
  };

  flake.homeModules.media = { pkgs, config, ... }:
  {
    services.mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/mus/mpd";
      extraConfig = ''
	audio_output {
          type "pipewire"
          name "My Pipewire Output"
          mixer_type "hardware"
          mixer_device "default"
          mixer_control "PCM"
        }
      '';
    };

    programs.emacs.init.usePackage.mpc-wrapper = {
      enable = true;
      extraPackages = [pkgs.mpc];
      generalOne.global-leader = {
	"m" = '''(:ignore t :which-key "mpd")'';
	"mp" = '''("replay file" . mpc-play)'';
	"mm" = '''("menu" . music-menu)'';
	"ms" = '''("stop" . mpc-stop)'';
	"m+" = "'mpc-inc-by-five";
	"m-" = "'mpc-dec-by-five";
      };
      deferIncrementally = true;
      config = ''
	(defun mpc-add (file)
	  "Add FILE to the queue. FILE is a string."
	  (shell-command (concat "mpc add " "\"" file "\"")))
	(defun music-menu ()
	  "Play music from a menu."
	  (interactive)
	  (mpc-clear)
	  (mpc-add (completing-read "Choose a song: " (mpc-ls)))
	  (mpc-play))
	(defun mpc-inc-by-five ()
	  "Increase the volume by 5."
	  (interactive)
	  (mpc-volume-inc 5))
	(defun mpc-dec-by-five ()
	  "Decrease the volume by 5."
	  (interactive)
	  (mpc-volume-dec 5))
	(repeaters-define-maps
	 '(("music"
	    mpc-volume-dec-by-five "-"
	    mpc-volume-inc-by-five "+")))
      '';
    };
  };
}
