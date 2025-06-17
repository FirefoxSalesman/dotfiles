{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.json.enable = lib.mkEnableOption "enables json support";

  config = lib.mkIf ide.languages.json.enable {
    programs.emacs.init.usePackage = {
      json-ts-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [vscode-langservers-extracted] else [];
        mode = [''"\\.json\\'"''];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
      };

      json5-ts-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [vscode-langservers-extracted] else [];
        mode = [''"\\.json5\\'"''];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
        config = ''
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '((json5-ts-mode) . ("vscode-json-language-server" "--stdio"))))
        '';
      };
    };
  };
}
