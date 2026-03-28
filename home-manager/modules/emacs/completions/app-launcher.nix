{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.app-launcher = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "app-launcher";
	version = "current";
	src = inputs.app-launcher;
      }
    );
  };
  
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage.app-launcher = {
      enable = true;
      defer = true;
      command = ["app-launcher-run-app"];
    };
  };
}
