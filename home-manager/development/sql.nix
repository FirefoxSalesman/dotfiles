{ pkgs, ... }:

{
  home.packages = with pkgs; [
    sqls
  ];

  programs.emacs.init.usePackage.sql = {
    enable = true;
    mode = [''"\\.sql\\'"''];
    eglot = true;
    symex = true;
  };
}
