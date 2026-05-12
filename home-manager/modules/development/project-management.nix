{
  flake.homeModules.development = { ... }:
  {
    programs.emacs.init = {
      ide = {
	magit = {
	  enable = true;
	  forge = false;
	  todo = false;
	};
	project = true;
      };
      usePackage = {
	magit = {
	  setopt.magit-process-find-password-functions = ["'magit-process-password-auth-source"];
	  generalOneConfig = {
	    magit-mode-map = {
	      "e" = "'evil-next-visual-line";
	      "B" = "'evil-goto-line";
	    };
	    magit-status-mode-map."j" = "'magit-unstage-files";
	  };
	  # https://github.com/magit/magit/issues/5557
	  config = ''
	    (defalias 'magit--any
              (static-if (fboundp 'member-if) #'member-if #'cl-member-if))
	  '';
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
