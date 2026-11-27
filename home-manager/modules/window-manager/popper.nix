{
  flake.homeModules.wm.programs.emacs.init.usePackage.popper = {
    enable = true;
    hook = [
      {
        names = [ "on-first-buffer" ];
        actions = [ "'popper-mode" ];
      }
    ];
    general = {
      "s-'" = "'popper-toggle";
      "s-\\\"" = "'popper-cycle";
      "C-s-'" = "'popper-toggle-type";
    };
    setopt = {
      popper-window-height = 30;
      popper-group-function = "'popper-group-by-project";
      popper-reference-buffers = [
        "'compilation-mode"
        "'inferior-python-mode"
        "'occur-mode"
        "'grep-mode"
        ''"^\\*prolog\\*"''
        "'xref--xref-buffer-mode"
        "'flymake-diagnostics-buffer-mode"
        "'rustic-cargo-test-mode"
        "'rustic-cargo-run-mode"
        "'racket-repl-mode"
        "'inferior-ess-r-mode"
        "'cider-repl-mode"
      ];
    };
  };
}
