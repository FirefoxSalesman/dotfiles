{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.semel = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "semel";
	version = "current";
	src = inputs.semel;
      }
    );
  };
}
