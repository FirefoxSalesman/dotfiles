{ pkgs, config, lib, ... } :

let
  ide = config.programs.emacs.init.ide;
in
{
  options = {
    programs.emacs.init.ide.languages.java = {
      enable = lib.mkEnableOption "enables java support";
      gradle = lib.mkEnableOption "enables groovy-mode, a major mode for editing gradle files";
    };
  };

  config = lib.mkIf ide.languages.java.enable {
    programs.emacs.init.usePackage = {
      java-ts-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [jdt-language-server] else [];
        mode = [''"\\.java\\'"''];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
      };

      groovy-mode = {
        enable = ide.languages.java.gradle;
        symex = ide.symex;
        mode = [''"\\.gradle\\'"''];
      };
    };
  };
}
