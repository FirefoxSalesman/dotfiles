{ pkgs, ... }:

{
  programs.emacs.init.usePackage = {
    html-ts-mode = {
      enable = true;
      extraPackages = with pkgs; [vscode-langservers-extracted];
      mode = [''"\\.[px]?html?\\'"''];
      eglot = true;
      symex = true;
    };

    emmet-mode = {
      enable = true;
      ghook = ["('(js-ts-mode-hook sgml-mode-hook css-ts-mode-hook html-ts-mode-hook) 'emmet-mode)"];
      custom.emmet-move-cursor-between-quotes = "t";
    };

    pug-mode = {
      enable = true;
      mode = [''"\\.pug\\'"''];
    };

    css-ts-mode = {
      enable = true;
      extraPackages = with pkgs; [vscode-langservers-extracted];
      mode = [''"\\.css\\'"''];
      eglot = true;
      symex = true;
    };

    js-ts-mode = {
      enable = true;
      extraPackages = [typescript-language-server];
      mode = [''"\\.js\\'"''];
      eglot = true;
      symex = true;
    };
  };
}
