{ pkgs, ... }:

{
  programs.emacs.init.usePackage.python-ts-mode = {
    enable = true;
    eglot = true;
    symex = true;
    mode = [''"\\.py\\'"''];
    extraPackages = with pkgs; [ python313Packages.python-lsp-server ];
    generalTwo."local-leader".python-mode-map."r" = "'python-shell-send-buffer";
    custom = {
      python-shell-interpreter = ''"ipython"'';
  	  python-shell-interpreter-args = ''"-i --simple-prompt"'';
    };
  };
}
