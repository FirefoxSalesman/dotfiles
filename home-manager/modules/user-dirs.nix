{
  flake.homeModules.main = { config, ... }: {
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
