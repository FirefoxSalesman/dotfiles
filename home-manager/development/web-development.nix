{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.web = {
    enable = lib.mkEnableOption "enables html, css, & js support";
    emmet = lib.mkEnableOption "enables emmet for html, js, & css";
    pug = lib.mkEnableOption "enables pug support";
  };

  config = lib.mkIf ide.languages.web.enable {
    programs.emacs.init.usePackage = {
      html-ts-mode = {
        enable = true;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then with pkgs; [vscode-langservers-extracted] else [];
        # many thanks to doom
        mode = [''"\\.[px]?html?\\'"''];
        config = ''
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '((html-ts-mode) . ("vscode-html-language-server" "--stdio"))))
        '';
        eglot = ide.eglot.enable;
        symex = ide.symex;
        lsp = ide.lsp.enable;
      };

      emmet-mode = {
        enable = ide.web.emmet;
        hook = [
          "(js-ts-mode . emmet-mode)"
          "(sgml-mode . emmet-mode)"
          "(css-ts-mode . emmet-mode)"
          "(html-ts-mode . emmet-mode)"
        ];
        custom.emmet-move-cursor-between-quotes = lib.mkDefault "t";
      };

      pug-mode = {
        enable = ide.web.pug;
        mode = [''"\\.pug\\'"''];
      };

      css-ts-mode = {
        enable = true;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then with pkgs; [vscode-langservers-extracted] else [];
        mode = [''"\\.css\\'"''];
        eglot = ide.eglot.enable;
        symex = ide.symex;
        lsp = ide.lsp.enable;
      };

      js-ts-mode = {
        enable = true;
        extraPackages = if ide.lsp.enable || ide.eglot.enable then with pkgs; [typescript-language-server] else [];
        mode = [''"\\.js\\'"''];
        eglot = ide.eglot.enable;
        symex = ide.symex;
        lsp = ide.lsp.enable;
      };
    };
  };
}
