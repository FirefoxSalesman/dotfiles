{ pkgs, ... }:

{
  programs.emacs.init.usePackage.sql = {
    enable = true;
    extraPackages = with pkgs; [sqls];
    mode = [''"\\.sql\\'"''];
    eglot = true;
    symex = true;
  };
}
