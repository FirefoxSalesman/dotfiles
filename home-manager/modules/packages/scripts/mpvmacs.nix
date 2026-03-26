{ inputs, ... }:

{
  perSystem = {pkgs, ... }: {
    packages.mpvmacs = pkgs.writeShellScriptBin "mpvmacs" ''
      emacsclient -e "(mpv-play \"$1\")"
    '';
  };
}
