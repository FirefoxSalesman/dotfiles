{
  flake.homeModules.aesthetics.programs.emacs.init = {
    keybinds.whichKey.posframe = {
      enable = false;
      unparent = true;
    };
    completions.vertico.posframe = false;
    usePackage.vertico-posframe.config = "(set-face-attribute 'vertico-posframe-face nil :family 'variable-pitch)";
  };
}
