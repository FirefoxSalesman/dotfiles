{ pkgs }:

pkgs.writeShellScriptBin "masstube" ''
  emacsclient ~/.download.txt
  # ${pkgs.yt-dlp}/bin/yt-dlp $1 --verbose -ci --batch-file=~/.download.txt --proxy socks://localhost:9050
  ${pkgs.yt-dlp}/bin/yt-dlp $1 --verbose -ci --batch-file=~/.download.txt 
  ${pkgs.coreutils}/bin/rm ~/.download.txt
''
