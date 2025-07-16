{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.eglot.preset = {
    enable = lib.mkEnableOption "Enable eglot's preset configuration";
    hoverDoc = lib.mkEnableOption "Use eldoc-box to get documentation popups";
    breadcrumb = lib.mkEnableOption "Enables the breadcrumb header";
  };

  config = lib.mkIf ide.eglot.preset.enable {
    programs.emacs.init.usePackage = {
      eglot = {
        enable = true;
        custom = {
          eglot-report-progress = "nil";
          eglot-autoshutdown = "t";
          #borrowed from doom
          eglot-sync-connect = "1";
        };
      };

      eglot-booster = {
        enable = true;
        extraPackages = [pkgs.emacs-lsp-booster];
        after = ["eglot"];
        config = "(eglot-booster-mode)";
      };

      eglot-x = {
        enable = true;
        after = ["eglot"];
        config = "(eglot-x-setup)";
      };

      eldoc-box = {
        enable = ide.eglot.preset.hoverDoc;
        hook = ["(eglot-managed-mode . eldoc-box-hover-at-point-mode)"];
      };

      breadcrumb = {
        enable = ide.eglot.preset.breadcrumb;
        hook = ["(eglot-managed-mode . breadcrumb-local-mode)"];
      };
    };
  };
}
