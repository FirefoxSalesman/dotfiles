{ lib, ... }:

{
  imports = [
    ./flymake.nix
  ];

  options.programs.emacs.init.ide.flymake.enable = lib.mkEnableOption "Enables flymake for any languages that benefit from it.";
}
