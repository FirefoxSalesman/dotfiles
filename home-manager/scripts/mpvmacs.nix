{ pkgs, ... }:

pkgs.writeShellScriptBin "mpvmacs" ''
  emacsclient -e "(mpv-play \"$1\")"
''
