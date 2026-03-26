{
  flake.homeModules.aesthetics = { ... }:
  {
    programs.emacs.init = {
      keybinds.whichKey.posframe = {
        enable = true;
        unparent = true;
      };
      usePackage.vertico-posframe = {
	enable = true;
	defer = true;
	ghookf = ["('vertico-mode 'vertico-posframe-mode)"];
	config = ''(set-face-attribute 'vertico-posframe-face nil :family 'variable-pitch)'';
      };
    };
  };
}
