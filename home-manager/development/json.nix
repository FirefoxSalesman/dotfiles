{ pkgs, ... }:

{
  home.packages = with pkgs; [
    vscode-langservers-extracted
  ];

  programs.emacs.init.usePackage = {
    json-ts-mode = {
      enable = true;
      mode = [''"\\.json\\'"''];
      eglot = true;
      symex = true;
    };

    json5-ts-mode = {
      enable = true;
      mode = [''"\\.json5\\'"''];
      eglot = true;
      symex = true;
    };
  };
}
