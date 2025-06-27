{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.zig.enable = lib.mkEnableOption "enables zig support";

  config = lib.mkIf ide.languages.zig.enable {
    programs.emacs.init.usePackage.zig-mode = {
      enable = true;
      extraPackages = if (ide.eglot || ide.lsp) then [pkgs.zls pkgs.zig] else [];
      mode = [''"\\.zig\\'"''];
      symex = ide.symex;
      lsp = ide.lsp;
      eglot = ide.eglot;
    };
  };
}
