{ inputs, ... } :

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages.qutebrowser-emacs = (epkgs.callPackage
      epkgs.trivialBuild rec {
	pname = "qutebrowser";
	version = "current";
	src = inputs.exwm-qutebrowser;

	propagatedUserEnvPkgs = with epkgs; [
	  consult
	  exwm
	  password-store
	  dash
	  evil
	  doom-modeline
	];

	buildInputs = propagatedUserEnvPkgs;
      }
    );
  };
}
