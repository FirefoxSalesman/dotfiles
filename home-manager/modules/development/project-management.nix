{
  flake.homeModules.development = { ... }:
  {
    programs.emacs.init = {
      ide = {
	magit = {
	  enable = true;
	  forge = true;
	  todo = true;
	};
	project = true;
      };
      usePackage = {
	magit = {
	  setopt.magit-process-find-password-functions = ["'magit-process-password-auth-source"];
	  generalOneConfig = {
	    magit-mode-map."e" = "'evil-next-visual-line";
	    magit-status-mode-map."j" = "'magit-unstage-files";
	  };
	};

	projection-ibuffer = {
	  enable = true;
	  generalOne.project-prefix-map.i = ''`("ibuffer" . ,(cmd! (ibuffer) (ibuffer-filter-by-projection-root (project-current))))'';
	};

	projection-multi.custom.projection-gradle-use-daemon = false;
      };
    };
  };
}
