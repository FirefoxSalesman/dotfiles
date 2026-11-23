{
  flake.homeModules.shellConfig.programs.emacs.init.usePackage = {
    ghostel = {
      enable = true;
      hook = [
        {
          names = [ "on-first-input" ];
          actions = [
            "'ghostel-compile-global-mode"
            "'ghostel-comint-global-mode"
          ];
        }
        {
          names = [
            "ghostel-mode"
            "ghostel-compile-view-mode"
          ];
          actions = [ "(lambda () (display-line-numbers-mode -1))" ];
        }
      ];
      generalTwoConfig.":n".ghostel-mode-map = {
        g = "'ghostel-yank";
        G = "'ghostel-yank";
      };
    };

    popper.setopt.popper-reference-buffers = [
      "'ghostel-mode"
      "'ghostel-compile-view-mode"
    ];
  };
}
