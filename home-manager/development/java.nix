{ pkgs, ... }:

{
  programs.emacs.init.usePackage = {
    java-ts-mode = {
      enable = true;
      mode = [''"\\.java\\'"''];
      eglot = true;
      symex = true;
    };

    groovy-mode = {
      enable = true;
      symex = true;
      mode = [''"\\.gradle\\'"''];
    };
  };
}
