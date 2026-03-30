{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      keybinds.evil.surround = true;
      usePackage.evil-surround.generalTwoConfig = {
	":v".evil-surround-mode-map = {
	  "r" = "'evil-surround-region";
	  "R" = "'evil-surround-region";
	};
	":o".evil-surround-mode-map = {
	  "s" = "nil";
	  "r" = "'evil-surround-edit";
	  "R" = "'evil-Surround-edit";
	};
      };
    };
  };
}
