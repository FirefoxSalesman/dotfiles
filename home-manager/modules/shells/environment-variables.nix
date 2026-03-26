{
  flake.homeModules.shellConfig = { lib, ... }:
  {
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
  };
}
