{ inputs, ... }:

{
  perSystem = { pkgs, ... }: {
    packages.i3status-rs = pkgs.writeShellScriptBin "i3status-rust" ''
      ${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml
    '';
  };
}
