{
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

        dashboard.setopt.dashboard-banner-logo-title = ''"Emacs: The one true desktop environment"'';
      };
    };
  };
}
