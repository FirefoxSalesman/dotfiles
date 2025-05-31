{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nixd
  ];

  programs.emacs.init.usePackage.nix-mode = {
    enable = true;
    mode = [''"\\.nix\\'"''];
    eglot = true;
    symex = true;
  };
}
