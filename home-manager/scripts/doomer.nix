{ pkgs }:

  pkgs.writeShellScriptBin "doomer" ''
    ${pkgs.sox}/bin/sox "$1" "$1.flac" pitch -500 treble -20 pad 0 3 reverb 80 tempo .8 highpass 1000 lowpass 700 compand 0.3,0.8 0
  ''
