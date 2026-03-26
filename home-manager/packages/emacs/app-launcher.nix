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
}
