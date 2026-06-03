{ inputs, ... }:

{
  perSystem = { pkgs, ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages = {
      avy-dashboard = (epkgs.callPackage
	epkgs.trivialBuild rec {
	pname = "avy-dashboard";
	version = "current";
	src = inputs.avy-dashboard;

	propagatedUserEnvPkgs = with epkgs; [
	  avy
	  dashboard
	];

	buildInputs = propagatedUserEnvPkgs;
	}
      );
    };
  } ;

  flake.homeModules.aesthetics = { ... }:

  {
    programs.emacs.init = {
      aesthetics = {
        dashboard.enable = true;
        icons = {
          enable = true;
          forceInstallAllTheIcons = true;
        };
      };
      usePackage = {
	prettify-symbols = {
	  enable = true;
	  ghookf = ["('prog-mode 'prettify-symbols-mode)"];
	};

        dashboard.setopt = {
	  dashboard-banner-logo-title = ''"Emacs: The one true desktop environment"'';
	  dashboard-startup-banner = "'logo";
	};

	avy-dashboard = {
	  enable = true;
	  generalTwo.":nm".dashboard-mode-map."H-a" = "'avy-dashboard";
	};
      };
    };
  };
}
