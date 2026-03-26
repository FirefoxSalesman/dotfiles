{
  flake.homeModules.shellConfig = { pkgs, lib, ... }:
  {
    programs = {
      direnv.enableBashIntegration = true;

      bash = {
	enable = true;
	initExtra = ''${lib.getExe pkgs.pfetch}'';
	shellAliases.z = "cd ./$(ls -d */ .*/ | ${lib.getExe pkgs.ezf})";
      };
    };
  } ;
}
