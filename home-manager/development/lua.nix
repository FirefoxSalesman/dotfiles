{ pkgs, ... }:

{
  home.packages = with pkgs; [
    lua-language-server
  ];

  programs.emacs.init.usePackage.lua-ts-mode = {
    enable = true;
    mode = [''"\\.lua\\'"''];
    eglot = true;
  };
}
