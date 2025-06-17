{ pkgs, config, lib, ... } :

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.nix.enable = lib.mkEnableOption "enables nix support (highly reccommended)";

  config = lib.mkIf ide.languages.nix.enable {
    programs.emacs.init.usePackage.nix-mode = {
      enable = true;
      mode = [''"\\.nix\\'"''];
      extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [nixd] else [];
      eglot = ide.eglot;
      lsp = ide.lsp;
      symex = ide.symex;
    };
  };
}
