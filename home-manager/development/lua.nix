{ pkgs, ... }:

{
  programs.emacs.init.usePackage.lua-ts-mode = {
    enable = true;
    extraPackages = with pkgs; [lua-language-server];
    mode = [''"\\.lua\\'"''];
    eglot = true;
  };
}
