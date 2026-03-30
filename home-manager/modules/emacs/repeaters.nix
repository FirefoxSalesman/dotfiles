{ inputs, ... }:
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

  flake.homeModules.emacs = { ... }: {
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.repeaters ];
      init = {
	prelude = ''
	  (use-package repeaters
	    :demand t
	    :config
	    (repeaters-define-maps
	     '(("next-error" ;; borrowed from the hydra wiki
	        next-error "`"
	        next-error "n"
	        previous-error "e"))))
	  
	  (use-package repeat
	    :config (repeat-mode)
	    :general-config ("H-z" 'repeat))
	'';
	usePackage.evil-collection.setopt.evil-collection-unimpaired-want-repeat-mode-integration = true;
      };
    };
  };
}
