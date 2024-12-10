{ pkgs }:

pkgs.writeShellScriptBin "cast" ''
  #Exit on fail
  set -e

  #Set Input & Output Filetypes
  in=$(${pkgs.coreutils}/bin/printf 'm4a\nopus\nmp4\nmkv\nwebm' | ezf)
  out=$(${pkgs.coreutils}/bin/printf 'mp3\nflac\nwav\nogg' | ezf)

  #Choose device to transfer to
  device=$(${pkgs.coreutils}/bin/ls /run/media/$USER | ezf)

  #Convert files
  ${(import ./ffmpeg-bulk.nix { inherit pkgs; })}/bin/ffmpeg-bulk *.$in -t $out
  ${pkgs.coreutils}/bin/rm *.$in

  #Remove metadata so the glowies don't see it.
  ${pkgs.python312Packages.mat2}/bin/mat2 *.$out

  #Transfer
  ${pkgs.coreutils}/bin/mv ~/*.cleaned.$out /run/media/$USER/$device/Podcasts
  ${pkgs.coreutils}/bin/rm ~/*.$out
''
