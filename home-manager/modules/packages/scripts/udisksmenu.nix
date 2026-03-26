{ inputs, ... }:

{
  perSystem = { pkgs, self', ... }: let ezf = self'.packages.ezf;
  in {
    packages.udisksmenu = pkgs.writeShellScriptBin "udisksmenu" ''
      action=$(${pkgs.coreutils}/bin/printf 'mount\nunmount' | ${ezf}/bin/ezf)
      disk=$(${pkgs.coreutils}/bin/ls /dev | ${pkgs.ripgrep}/bin/rg sd[a-z] | ${ezf}/bin/ezf)

      ${pkgs.udisks}/bin/udisksctl $action -b /dev/$disk
    '';
  };
}
