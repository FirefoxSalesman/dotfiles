{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage.no-littering = {
      enable = true;
      demand = true;
      #no-littering doesn't set this by default so we must place
      #auto save files in the same path as it uses for sessions
      setopt.auto-save-file-name-transforms = ''`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))'';
    };
  };
}
