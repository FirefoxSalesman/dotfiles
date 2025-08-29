{ pkgs, lib, config, ... }:

let
  keybinds = config.programs.emacs.init.keybinds;
in
{
  options.programs.emacs.init.keybinds.doomEscape.enable = lib.mkEnableOption "Enables doom escape, as seen in doom emacs";

  config.programs.emacs.init.usePackage.doom-escape = lib.mkIf keybinds.doomEscape.enable {
    enable = true;
    package = epkgs: epkgs.doom-utils;
    bind."C-g" = "doom/escape";
    generalOne."'normal"."<escape>" = lib.mkIf keybinds.evil.enable "'doom/escape";
  };
}
