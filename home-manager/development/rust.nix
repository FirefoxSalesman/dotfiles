{ pkgs, ... }:

{
  programs.emacs.init.usePackage = {
    rust-ts-mode = {
      enable = true;
      defer = true;
      eglot = true;
      symex = true;
    };

    rustic = {
      enable = true;
      custom = {
        rust-mode-treesitter-derive = "t";
        rustic-lsp-client = "'eglot";
      };
    };
  };
}
