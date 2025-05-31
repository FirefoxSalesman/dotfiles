{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nodePackages.bash-language-server
  ];

  programs.emacs.init.usePackage.bash-ts-mode = {
    enable = true;
    mode = [''"\\.sh\\'"''];
    eglot = true;
  };
}
