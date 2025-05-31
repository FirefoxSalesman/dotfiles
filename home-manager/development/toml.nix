{ pkgs, ... }:

{
  programs.emacs.init.usePackage.toml-ts-mode = {
    enable = true;
    mode = [''"\\.toml\\'"''];
    symex = true;
  };
}
