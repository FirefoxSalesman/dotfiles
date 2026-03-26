{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.emacs-ezf = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "ezf";
	version = "current";
	src = inputs.ezf;
      }
    );
  };
}
