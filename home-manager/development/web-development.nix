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
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [vscode-langservers-extracted] else [];
        # many thanks to doom
        mode = [''"\\.[px]?html?\\'"''];
        config = ''
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '((html-ts-mode) . ("vscode-html-language-server" "--stdio"))))
        '';
        eglot = ide.eglot;
        symex = ide.symex;
        lsp = ide.lsp;
      };

      emmet-mode = {
        enable = ide.web.emmet;
        ghook = ["('(js-ts-mode-hook sgml-mode-hook css-ts-mode-hook html-ts-mode-hook) 'emmet-mode)"];
        custom.emmet-move-cursor-between-quotes = "t";
      };

      pug-mode = {
        enable = ide.web.pug;
        mode = [''"\\.pug\\'"''];
      };

      css-ts-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [vscode-langservers-extracted] else [];
        mode = [''"\\.css\\'"''];
        eglot = ide.eglot;
        symex = ide.symex;
        lsp = ide.lsp;
      };

      js-ts-mode = {
        enable = true;
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [typescript-language-server] else [];
        mode = [''"\\.js\\'"''];
        eglot = ide.eglot;
        symex = ide.symex;
        lsp = ide.lsp;
      };
    };
  };
}
