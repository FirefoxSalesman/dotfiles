{ inputs, ... }:

{
  perSystem = { pkgs, self', ... }: let ezf = self'.packages.ezf;
  in {
    packages.udisksmenu = pkgs.writeShellScriptBin "udisksmenu" ''
      action=$(${pkgs.coreutils}/bin/printf 'mount\nunmount' | ${ezf}/bin/ezf)
      disk=$(${pkgs.coreutils}/bin/ls /dev | ${pkgs.ripgrep}/bin/rg sd[a-z] | ${ezf}/bin/ezf)

      ${pkgs.udisks}/bin/udisksctl $action -b /dev/$disk
    '';
  };

  flake.homeModules.emacs = { pkgs, lib, ... }: {
    programs.emacs.init = {
      keybinds.leader-key = {
        enable = true;
        globalPrefix = "s";
      };
      usePackage.emacs.generalOneConfig.global-leader = {
	"l" = ''`("Compile" . ,(cmd! (if (project-current) (project-compile) (compile (read-string "Compile command: " "make -k")))))'';
	"L" = ''`("Recompile" . ,(cmd! (if (project-current) (project-recompile) (recompile))))'';
	"u" = ''`("Mount USB" . ,(cmd! (start-process-shell-command "udisksmenu" nil "${lib.getExe pkgs.udisksmenu}")))'';
      };
    };
  };
}
