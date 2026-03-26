{
  flake.homeModules.shellConfig = { pkgs, lib, ... }:
  {
    home.shellAliases = {
      ffrecord = "ffmpeg -f sndio -i snd/0.mon -f x11grab -r 30 -s 1920x1080 -i :0 -c:v libx164 -preset ultrafast -acodec copy ~/test.mkv";
      ffaud = "ffmpeg -f alsa -channels 1 -sample_rate 44100 -i default:CARD=Mic output.flac";
      sx = "startx";
      otp = "pass otp";
      run = "cd /run/";
      l = "ls";
      ".." = "cd ..";
      tortube = "yt-dlp --proxy socks://localhost:9050";
      wget = "torsocks wget --hsts-file=$XDG_DATA_HOME/wget-hsts";
      ga = "git add";
      gc = "torsocks git clone";
      gp = "git pull";
      gP = "git push";
    };
  };
}
