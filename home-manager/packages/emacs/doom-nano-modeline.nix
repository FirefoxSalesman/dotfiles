{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.doom-nano-modeline = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "doom-nano-modeline";
	version = "current";
	src = inputs.doom-nano-modeline;

	propagatedUserEnvPkgs = [
	  epkgs.doom-themes
	];

	buildInputs = propagatedUserEnvPkgs;
      }
    );
};
}
