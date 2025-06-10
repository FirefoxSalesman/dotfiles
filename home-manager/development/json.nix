{ pkgs, ... }:

{
  programs.emacs.init.usePackage = {
    json-ts-mode = {
      enable = true;
      extraPackages = with pkgs; [vscode-langservers-extracted];
      mode = [''"\\.json\\'"''];
      eglot = true;
      symex = true;
    };

    json5-ts-mode = {
      enable = true;
      extraPackages = with pkgs; [vscode-langservers-extracted];
      mode = [''"\\.json5\\'"''];
      eglot = true;
      symex = true;
    };
  };
}
