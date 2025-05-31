{ pkgs, ... }:

{
  programs.emacs.init.usePackage.c-ts-mode = {
    enable = true;
    mode = [''"\\.c\\'"''];
    eglot = true;
    symex = true;
  };
}
