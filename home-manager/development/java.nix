{ pkgs, config, lib, ... } :

let
  ide = config.programs.emacs.init.ide;
in
{
  options = {
    programs.emacs.init.ide.languages.java = {
      enable = lib.mkEnableOption "enables java support";
      gradle = lib.mkEnableOption "enables groovy-mode, a major mode for editing gradle files";
      clojure = lib.mkEnableOption "enables clojure support";
      scala = lib.mkEnableOption "Enables scala support. You will need to bring your own copy of sbt in order to use sbt-mode";
    };
  };

  config = lib.mkIf ide.languages.java.enable {
    programs.emacs.init.usePackage = {
      java-ts-mode = {
        enable = true;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then with pkgs; [jdt-language-server] else [];
        mode = [''"\\.java\\'"''];
        lsp = ide.lsp.enable;
        eglot = ide.eglot.enable;
        symex = ide.symex;
      };

      groovy-mode = {
        enable = ide.languages.java.gradle;
        symex = ide.symex;
        mode = lib.mkDefault [''"\\.gradle\\'"''];
      };
      
      clojure-mode = {
        enable = ide.languages.java.clojure;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then with pkgs; [clojure-lsp] else [];
        mode = [''"\\.clj\\'"''];
        lsp = ide.lsp.enable;
        eglot = ide.eglot.enable;
        symex = ide.symex;
      };

      cider = {
        enable = ide.languages.java.clojure;
        hook = ["(clojure-mode . cider-mode)"];
      };
      
      scala-ts-mode = {
        enable = ide.languages.java.scala;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then [pkgs.metals] else [];
        mode = [''"\\.scala\\'"''];
        eglot = ide.eglot.enable;
        lsp = ide.lsp.enable;
        symex = ide.symex;
        config = ''
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '((scala-ts-mode) . ("metals"))))
        '';
      };

      sbt-mode = {
        enable = ide.languages.java.scala;
        after = ["scala-ts-mode"];
      };
    };
  };
}
