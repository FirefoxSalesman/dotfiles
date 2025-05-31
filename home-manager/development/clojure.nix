{ pkgs, ... }:

{
  imports = [
    ./java.nix
  ];

  home.packages = with pkgs; [
    clojure-lsp
  ];
  
  programs.emacs.init.usePackage = {
    clojure-mode = {
      enable = true;
      mode = [''"\\.clj\\'"''];
      eglot = true;
      symex = true;
    };

    cider = {
      enable = true;
      ghook = ["('clojure-mode-hook 'cider-mode)"];
      generalTwo.local-leader.cider-mode-map = {
        "s" = '''(cider-jack-in :which-key "start cider")''; 
      };
    };
  };
}
