{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.lsp-bridge.preset = lib.mkEnableOption "Enable lsp-bridge's preset configuration. (A total of 1 changed variable)";

  config = lib.mkIf ide.lsp-bridge.preset {
    programs.emacs.init.usePackage = {
      lsp-bridge = {
        enable = true;
        custom = {
          lsp-bridge-get-workspace-folder = lib.mkDefault "'project-root";
          lsp-bridge-enable-org-babel = lib.mkDefault "t";
        };
      };
    };
  };
}
