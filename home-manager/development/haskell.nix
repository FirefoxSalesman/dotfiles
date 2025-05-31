{ pkgs, ... }:

{
  programs.emacs.init.usePackage.haskell-mode = {
    enable = true;
    mode = [''"\\.hs\\'"''];
    eglot = true;
    symex = true;
  };
}
