{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      aesthetics.declutter.enable = true;
      tools.noLittering.enable = true;
      usePackage = {
	simple = {
	  enable = true;
	  setopt.save-interprogram-paste-before-kill = true;
	};

	display-line-numbers = {
	  enable = true;
	  setopt = {
	    display-line-numbers-type = "'relative";
	    display-line-numbers-width = 3;
	  };
	  config = "(global-display-line-numbers-mode)";
	  #Disable line numbers for some modes
	  ghookf = ["((gen-mode-hooks '(org term dired eww eat markdown help helpful Info Man shell pdf-view elfeed-search elfeed-show eshell racket-repl sage-shell nov)) (lambda () (display-line-numbers-mode 0)))"];
	} ;

	server = {
	  enable = true;
	  deferIncrementally = true;
	  config = "(server-start)";
	};

	super-save = {
	  enable = true;
	  ghookf = ["('on-first-file 'super-save-mode)"];
	  setopt = {
	    super-save-auto-save-when-idle = true;
	    auto-save-default = false;
	    super-save-silent = true;
	    super-save-delete-trailing-whitespace = true;
	  };
	};
      };
    };
  };
}
