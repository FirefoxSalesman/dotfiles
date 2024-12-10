{ pkgs }:

pkgs.writeShellScriptBin "udisksmenu" ''
action=$(${pkgs.coreutils}/bin/printf 'mount\nunmount' | ${(import ./ezf.nix { inherit pkgs; })}/bin/ezf)
disk=$(${pkgs.coreutils}/bin/ls /dev | ${pkgs.ripgrep}/bin/rg sd[a-z] | ${(import ./ezf.nix { inherit pkgs; })}/bin/ezf)

${pkgs.udisks}/bin/udisksctl $action -b /dev/$disk
''
