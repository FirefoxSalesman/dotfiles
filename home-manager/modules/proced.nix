{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage.proced = {
      enable = true;
      command = ["proced"];
      generalTwoConfig.":n".proced-mode-map = {
	"j" = "'proced-unmark";
	"k" = "'proced-send-signal";
      };
    };
  };
}
