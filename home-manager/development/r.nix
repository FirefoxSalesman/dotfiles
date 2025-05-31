{ pkgs, ... }:

{
  programs.emacs.init.usePackage.ess-r-mode = {
    enable = true;
    package = epkgs: epkgs.ess;
    mode = [''"\\.R\\'"''];
    eglot = true;
    symex = true;
    custom.ess-ask-for-ess-directory = "nil";
  };
}
