{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.scala.enable = lib.mkEnableOption "enables scala support";

  config = lib.mkIf ide.languages.scala.enable {
    programs.emacs.init.usePackage.scala-ts-mode = {
      enable = true;
      extraPackages = if (ide.lsp || ide.eglot) then [pkgs.metals] else [];
      mode = [''"\\.scala\\'"''];
      eglot = ide.eglot;
      lsp = ide.lsp;
      symex = ide.symex;
    };
  };
}
