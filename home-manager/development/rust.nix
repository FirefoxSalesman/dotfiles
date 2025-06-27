{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.rust.enable = lib.mkEnableOption "enables rust support";

  config = lib.mkIf ide.languages.rust.enable {
    programs.emacs.init.usePackage = {
      rust-ts-mode = {
        enable = true;
        defer = true;
        extraPackages = if ide.lsp || ide.eglot then [pkgs.rust-analyzer] else [];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
      };

      rustic = {
        enable = true;
        mode = [''("\\.rs$" . rustic-mode)''];
        custom = {
          rust-mode-treesitter-derive = "t";
          rustic-lsp-client = if ide.eglot then "'eglot" else
            if ide.lsp then "'lsp-mode" else
              "nil";
        };
      };
    };
  };
}
