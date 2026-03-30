{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage.wgrep = {
      enable = true;
      setopt.wgrep-auto-save-buffer = true;
      generalTwo.":n".grep-mode-map."w" = "'wgrep-change-to-wgrep-mode";
    };
  };
}
