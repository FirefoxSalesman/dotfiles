{ lib, ... }:

{
  imports = [
    ./eglot.nix
    ./lsp-mode.nix
    ./lsp-bridge.nix
  ];

  options.programs.emacs.init.ide.hoverDoc = lib.mkEnableOption "Enables hover documentation";
}
