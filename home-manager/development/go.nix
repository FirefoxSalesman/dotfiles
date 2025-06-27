{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.go.enable = lib.mkEnableOption "enables go support";

  config = lib.mkIf ide.languages.go.enable {
    programs.emacs.init.usePackage.go-ts-mode = {
      enable = true;
      mode = [''"\\.go\\'"''];
      symex = ide.symex;
      lsp = ide.lsp;
      eglot = ide.eglot;
      extraPackages = if (ide.lsp || ide.eglot) then [pkgs.gopls pkgs.go] else [];
    };
  };
}
