{
  flake.homeModules.c = { ... }:
  {
    programs.emacs.init = {
      ide.languages.c.enable = true;
      usePackage.c-ts-mode.generalTwoConfig.":n".c-ts-mode-map = {
	"S" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-below "comment"))'';
	"R" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-above "comment"))'';
	"o" = "'evil-previous-visual-line";
	"O" = "'evil-scroll-up";
      };
    };
  };
}
