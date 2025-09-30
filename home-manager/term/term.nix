{ config, lib, ... }:

let
  terminals = config.programs.emacs.init.terminals;
in
{
  options.programs.emacs.init.terminals.term = lib.mkEnableOption "Largely just installs multi-term. Largely stolen from doom.";

  config.programs.emacs.init.usePackage.multi-term = lib.mkIf terminals.term {
    enable = true;
    custom = {
      multi-term-dedicated-window-height = lib.mkDefault 20;
      multi-term-switch-after-close = lib.mkDefault "'PREVIOUS";
    };
    hook = ["(term-mode . (lambda () (setq-local hscroll-margin 0)))"];
  };
}
