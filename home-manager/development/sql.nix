{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.sql.enable = lib.mkEnableOption "enables sql support";

  config = lib.mkIf ide.languages.sql.enable {
    programs.emacs.init.usePackage.sql = {
      enable = true;
      extraPackages = [pkgs.sqls];
      mode = [''"\\.sql\\'"''];
      eglot = ide.eglot;
      lsp = ide.lsp;
      symex = ide.symex;
    };
  };
}
