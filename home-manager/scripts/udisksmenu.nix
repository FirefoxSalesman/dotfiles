{ pkgs }:

pkgs.writeShellScriptBin "udisksmenu" ''
action=$(${pkgs.coreutils}/bin/printf 'mount\nunmount' | ${pkgs.ezf}/bin/ezf)
disk=$(${pkgs.coreutils}/bin/ls /dev | ${pkgs.ripgrep}/bin/rg sd[a-z] | ${pkgs.ezf}/bin/ezf)

${pkgs.udisks}/bin/udisksctl $action -b /dev/$disk
''
