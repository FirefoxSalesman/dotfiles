{
  flake.homeModules.main = { lib, config, ... }: {
    home.sessionVariables = {
        XDG_DATA_HOME = "$HOME/.local";
        XDG_STATE_HOME = "$HOME/.local/state";
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_DIRS = lib.mkForce "$HOME/.nix-profile/share:$XDG_DATA_DIRS";
    };
    xdg.userDirs = {
      enable = true;
      setSessionVariables = true;
      createDirectories = true;
      desktop = null;
      publicShare = null;
      templates = null;
      documents = "${config.home.homeDirectory}/doc";
      download = "${config.home.homeDirectory}/dwn";
      music = "${config.home.homeDirectory}/mus";
      pictures = "${config.home.homeDirectory}/pic";
      videos = "${config.home.homeDirectory}/vid";
      projects = "${config.home.homeDirectory}/projects";
    };
  };
}
