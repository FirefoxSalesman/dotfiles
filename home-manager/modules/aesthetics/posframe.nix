{
  flake.homeModules.aesthetics =
    { ... }:
    {
      programs.emacs.init = {
        keybinds.whichKey.posframe = {
          enable = true;
          unparent = true;
        };
        completions.vertico.posframe = true;
        usePackage.vertico-posframe.config = "(set-face-attribute 'vertico-posframe-face nil :family 'variable-pitch)";
      };
    };
}
