{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.mpc-wrapper = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "mpc-wrapper";
	version = "current";
	src = inputs.mpc-wrapper;
      }
    );
  };
}
