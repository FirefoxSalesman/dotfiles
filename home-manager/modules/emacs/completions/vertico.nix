{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      completions = {
	vertico.enable = true;
	prescient = true;
	orderless = true;
      };
      usePackage = {
	vertico = {
	  generalTwoConfig.":n".vertico-map = {
	    "C-o" = "'vertico-scroll-down";
	    "C-e" = "'vertico-scroll-up";
	    "j" = "'evil-undo";
	    "I" = "'vertico-last";
	    "N" = "'vertico-first";
	    "B" = "'vertico-last";
	    "bg" = "'vertico-first";
	    "G" = "'evil-paste-after";
	  };
	  config = ''
	    (with-eval-after-load 'evil-collection-vertico
	      (efs/evil-collection-remap 'evil-collection-vertico-setup 'normal vertico-map
	    			     "k" 'evil-delete-char))
	  '';
	};

	vertico-quick.setopt = {
	  vertico-quick1 = ''"crst"'';
	  vertico-quick2 = ''"neia"'';
	};
      };
    };
  };
}
