{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options = {
    programs.emacs.init.ide.languages.python = {
      enable = lib.mkEnableOption "enables python support";
      jupyter = lib.mkEnableOption "enables code-cells, a minor mode for editing jupyter files";
    };
  };

  config = lib.mkIf ide.languages.python.enable {
    programs.emacs.init.usePackage = {
      python-ts-mode = {
        enable = true;
        eglot = ide.eglot;
        symex = ide.symex;
        lsp = ide.lsp;
        mode = [''"\\.py\\'"''];
        extraPackages = if (ide.lsp || ide.eglot) then with pkgs; [ python313Packages.python-lsp-server ] else [];
      };

      code-cells = {
        enable = ide.languages.python.jupyter;
        demand = true;
        extraPackages = with pkgs; [python313Packages.jupytext];
      };
    };
  };
}
