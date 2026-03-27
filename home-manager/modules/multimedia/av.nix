{
  flake.homeModules.media = { pkgs, config, ... }: {
    home.packages = with pkgs; [
      (config.lib.nixGL.wrap obs-studio)
      lmms
      audacity
      doomer
    ];
  };
}
