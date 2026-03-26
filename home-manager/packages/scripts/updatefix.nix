{ inputs, ... }:

{
  perSystem = { pkgs, ... }: {
    packages.updatefix = pkgs.writeShellScriptBin "updatefix" ''
      cp /usr/lib/systemd/system/getty@.bak /usr/lib/systemd/system/getty@.service
    '';
  };
}
