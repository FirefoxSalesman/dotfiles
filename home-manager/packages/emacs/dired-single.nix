{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.dired-single = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "dired-single";
	version = "current";
	src = inputs.dired-single;
      }
    );
  };
}
