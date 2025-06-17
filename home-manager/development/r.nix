{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.r.enable = lib.mkEnableOption "enables r support";

  config = lib.mkIf ide.languages.r.enable {
    programs.emacs.init.usePackage.ess-r-mode = {
      enable = true;
      package = epkgs: epkgs.ess;
      mode = [''"\\.R\\'"''];
      eglot = ide.eglot;
      lsp = ide.lsp;
      symex = ide.symex;
      custom.ess-ask-for-ess-directory = "nil";
    };
  };
}
