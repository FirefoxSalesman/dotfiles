{ pkgs, ... }:

{
  programs.emacs.init.usePackage.nix-mode = {
    enable = true;
    mode = [''"\\.nix\\'"''];
    extraPackages = with pkgs; [nixd];
    eglot = true;
    symex = true;
  };
}
