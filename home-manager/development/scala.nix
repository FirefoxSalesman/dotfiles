{ pkgs, ... }:

{
  programs.emacs.init.usePackage.scala-ts-mode = {
    enable = true;
    mode = [''"\\.scala\\'"''];
    eglot = true;
    symex = true;
  };
}
