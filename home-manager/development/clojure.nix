{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.clojure.enable = lib.mkEnableOption "enables clojure support";

  config = lib.mkIf ide.languages.clojure.enable {
    programs.emacs.init.usePackage = {
      clojure-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [clojure-lsp] else [];
        mode = [''"\\.clj\\'"''];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
      };

      cider = {
        enable = true;
        hook = ["(clojure-mode . cider-mode)"];
      };
    };
  };
}
