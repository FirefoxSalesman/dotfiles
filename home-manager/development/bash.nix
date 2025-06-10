{ pkgs, ... }:

{
  programs.emacs.init.usePackage.bash-ts-mode = {
    enable = true;
    extraPackages = with pkgs; [nodePackages.bash-language-server];
    mode = [''"\\.sh\\'"''];
    eglot = true;
  };
}
