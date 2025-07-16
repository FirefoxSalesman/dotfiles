{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.lsp.preset = {
    enable = lib.mkEnableOption "Enable lsp-mode's preset configuration (borrowed from doom)";
    breadcrumb = lib.mkEnableOption "Enables the breadcrumb header";
    ui = lib.mkEnableOption "Enables lsp-ui";
  };

  config = lib.mkIf ide.lsp.preset.enable {
    programs.emacs.init.usePackage = {
      # We elect not to use lsp-booster since we are meant to be on emacs 30, & that causes funky things to happen to lsp-mode
      lsp-mode = {
        enable = true;
        custom = {
          lsp-enable-folding = lib.mkDefault "nil";
          lsp-enable-text-document-color = lib.mkDefault "nil";
          lsp-enable-on-type-formatting = lib.mkDefault "nil";
          lsp-headerline-breadcrumb-enable = lib.mkDefault (if ide.lsp.preset.breadcrumb then "t" else "nil");
        };
      };

      lsp-ui = lib.mkIf ide.lsp.preset.ui {
        enable = true;
        hook = ["(lsp-mode . lsp-ui-mode)"];
        custom = {
          lsp-ui-doc-show-with-mouse = lib.mkDefault "nil";
          lsp-ui-doc-position = lib.mkDefault "'at-point";
          lsp-ui-doc-show-with-cursor = lib.mkDefault "t";
          lsp-ui-sideline-ignore-duplicate = lib.mkDefault "t";
          lsp-ui-sideline-show-hover = lib.mkDefault "nil";
          lsp-ui-sideline-actions-icon = lib.mkDefault "lsp-ui-sideline-actions-icon-default";
        };
      };
    };
  };
}
