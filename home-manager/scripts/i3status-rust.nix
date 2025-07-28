  { pkgs }:

  pkgs.writeShellScriptBin "i3status-rust" ''
  ${pkgs.i3status-rust}/bin/i3status-rs config-default.toml
  ''
