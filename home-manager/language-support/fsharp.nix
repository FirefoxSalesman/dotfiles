{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.fsharp.enable = lib.mkEnableOption "Enables support for fsharp. Borrowed from doom";

  config.programs.emacs.init.usePackage = lib.mkIf ide.languages.fsharp.enable {
    fsharp-mode = {
      enable = true;
      mode = [''"\\.fs[iylx]?\\'"''];
      extraPackages = lib.mkIf (ide.lsp.enable || ide.lspce.enable) [pkgs.fsautocomplete];
      eglot = ide.eglot.enable;
      lsp = ide.lsp.enable;
      lspce = ide.lspce.enable;
      babel = lib.mkIf ide.languages.org.enable "fsharp";
    };

    ob-fsharp = lib.mkIf ide.languages.org.enable {
      enable = true;
      after = ["org"];
    };

    eglot-fsharp = lib.mkIf ide.eglot.enable {
      enable = true;
      after = ["fsharp-mode"];
      custom = {
        eglot-fsharp-server-path = ''"${pkgs.fsautocomplete}/bin/"'';
        eglot-fsharp-server-install-dir = false;
      };
    };
  };
}
