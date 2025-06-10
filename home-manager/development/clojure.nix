{ pkgs, ... }:

{
  imports = [
    ./java.nix
  ];

  programs.emacs.init.usePackage = {
    clojure-mode = {
      enable = true;
      extraPackages = with pkgs; [clojure-lsp];
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
