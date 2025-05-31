{ lib, config, inputs, ... }:

{
  # This is some text
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "holschcc";
  home.homeDirectory = "/home/holschcc";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports =
    [ # Include the results of the hardware scan.
      ./internet.nix
      ./multimedia.nix
      ./aesthetics.nix
      ./extra-packages.nix
      # ./development.nix
      ./development
      ./gui.nix
      ./emacs.nix
      ./shells.nix
      ./gptel.nix
      ./sage.nix
      ./keyboard.nix
    ];

  nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  nixGL.packages = inputs.nixgl.packages;
  
  targets.genericLinux.enable = true;
  
  home.activation = {
    clearNixglCache = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        [ -v DRY_RUN ] || rm -f ${config.xdg.cacheHome}/nixgl/result*
      '';
  };
  

  home.file.".local/share/gnupg/gpg-agent.conf".text = ''
     pinentry-program /usr/bin/pinentry-emacs
     allow-loopback-pinentry
     allow-emacs-pinentry
     default-cache-ttl 600
     max-cache-ttl 7200
     enable-ssh-support
  '';
  home.file.".local/share/gnupg/gpg.conf".text = ''
     use-agent
  '';

  home.file.".alsoftrc".text = ''
    [general]
    drivers=pulse
    hrtf=true
  '';

  home.sessionVariables = {
    XDG_DATA_HOME="$HOME/.local";
    XDG_STATE_HOME="$HOME/.local/state";
    XDG_CACHE_HOME="$HOME/.cache";
    XDG_CONFIG_HOME="$HOME/.config";
    XDG_DATA_DIRS= lib.mkForce "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
    # EDITOR = "emacsclient";
    BROWSER="qutebrowser";
    _JAVA_OPTIONS="-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java";
    GTK2_RC_FILES="/home/holschcc/.gtkrc-2.0";
    SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    PATH="/run/system-manager/sw/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/holschcc/bin:/home/holschcc/.nix-profile/bin";
  };

  xdg.userDirs = {
      enable = true;
      createDirectories = true;
      desktop = null;
      publicShare = null;
      templates = null;
      documents = "${config.home.homeDirectory}/doc";
      download = "${config.home.homeDirectory}/dwn";
      music = "${config.home.homeDirectory}/mus";
      pictures = "${config.home.homeDirectory}/pic";
      videos = "${config.home.homeDirectory}/vid";
    };
}
