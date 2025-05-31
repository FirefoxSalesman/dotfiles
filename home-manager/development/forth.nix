{ pkgs, ... }:

{
  programs.emacs.init.usePackage.forth-mode = {
    enable = true;
    mode = [''"\\.fs\\'"''];
    symex = true;
  };
}
