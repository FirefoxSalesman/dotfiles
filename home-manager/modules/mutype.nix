{
  flake.homeModules.mutype = { ... }: {
    programs.emacs.init.usePackage.mutype = {
      enable = true;
      command = ["mutype-mode" "mutype-mode-custom"];
      setopt.mutype-source-directory = ''"~/.config/emacs/mutype"'';
    };
  };
}
