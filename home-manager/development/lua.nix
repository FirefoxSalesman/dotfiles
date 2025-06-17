{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.lua.enable = lib.mkEnableOption "enables lua support";

  config = lib.mkIf ide.languages.lua.enable {
    programs.emacs.init.usePackage.lua-ts-mode = {
      enable = true;
      extraPackages = if (ide.eglot || ide.lsp) then [pkgs.lua-language-server] else [];
      mode = [''"\\.lua\\'"''];
      eglot = ide.eglot;
      lsp = ide.lsp;
    };
  };
}
