{ pkgs, ... }:

pkgs.writeShellScriptBin "updatefix" ''
  dracut -f --regenerate-all
  cp /usr/lib/systemd/system/getty@.bak /usr/lib/systemd/system/getty@.service
''
