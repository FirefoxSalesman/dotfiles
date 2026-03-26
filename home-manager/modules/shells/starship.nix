{
  flake.homeModules.shellConfig = { ... }:
  {
    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      enableBashIntegration = true;
    };
  };
}
