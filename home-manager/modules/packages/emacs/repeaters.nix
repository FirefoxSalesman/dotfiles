{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.repeaters = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "repeaters";
	version = "current";
	src = inputs.repeaters;
      }
    );
  };
}
