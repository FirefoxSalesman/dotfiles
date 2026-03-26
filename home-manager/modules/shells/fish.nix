{
  flake.homeModules.shellConfig = { lib, pkgs, ... }:
  {
    programs.fish = {
      enable = true;
      package = pkgs.fish;
      interactiveShellInit = ''${lib.getExe pkgs.pfetch}'';
    };
  };
}
