{ pkgs, config, inputs, ... }:

{
  home.packages = with pkgs; [
    yt-dlp
    (config.lib.nixGL.wrap obs-studio)
    (config.lib.nixGL.wrap kdenlive)
    lmms
    mpc-cli
    (import ./scripts/doomer.nix { inherit pkgs; })
    (import ./scripts/masstube.nix { inherit pkgs; }) # Doesn't have its tor dependency declared.
    (import ./scripts/cast.nix { inherit pkgs; })
  ];

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
    package = epkgs: (epkgs.callPackage ./emacs/emacs-packages/mpc-wrapper.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });    
    generalOne."efs/leader-keys" = {
      "m" = '''(:ignore t :which-key "mpd")'';
      "mp" = '''(mpc-play :which-key "replay file")'';
      "mm" = '''(music-menu :which-key "menu")'';
      "ms" = '''(mpc-stop :which-key "stop")'';
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
      
    '';
  };

  programs.emacs.init.usePackage.mpv = {
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
  
  programs.mpv = {
    enable = true;
    package = pkgs.mpv;
    scripts = with pkgs.mpvScripts; [
      thumbnail
      sponsorblock
    ];
    config = {
      fs = "no";
      hwdec = "auto";
      x11-bypass-compositor = "no";
      volume-max = "250";
    };
  };

  programs.emacs.init.usePackage = {
    pulseaudio-control = {
      enable = true;
      custom.pulseaudio-control-volume-step = ''"5%"'';
      general = {
        "<XF86AudioRaiseVolume>" = "'pulseaudio-control-increase-sink-volume";
        "<XF86AudioLowerVolume>" = "'pulseaudio-control-decrease-sink-volume";
        "<XF86AudioMute>" = "'pulseaudio-control-toggle-current-sink-mute";
        "s-v" = "'pulseaudio-control-default-sink-mode";
      };
    };
  };
}
