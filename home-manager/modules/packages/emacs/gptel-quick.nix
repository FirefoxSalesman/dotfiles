{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.gptel-quick = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "gptel-quick";
	version = "current";
	src = inputs.gptel-quick;

	propagatedUserEnvPkgs = [
	  epkgs.gptel
	];

	buildInputs = propagatedUserEnvPkgs;
      }
    );
  };
}
