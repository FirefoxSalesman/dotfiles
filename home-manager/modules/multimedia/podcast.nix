{
  perSystem = { pkgs, self', ... }: let packages = self'.packages;
  in {
    packages = {
      cast = (pkgs.writeShellScriptBin "cast" ''
	#Exit on fail
	set -e
	
	#Set Input & Output Filetypes
	in=$(${pkgs.coreutils}/bin/printf 'm4a\nopus\nmp4\nmkv\nwebm' | ${packages.ezf}/bin/ezf)
	out=$(${pkgs.coreutils}/bin/printf 'mp3\nflac\nwav\nogg' | ${packages.ezf}/bin/ezf)
	
	#Choose device to transfer to
	device=$(${pkgs.coreutils}/bin/ls /run/media/$USER | ${packages.ezf}/bin/ezf)
	
	#Convert files
	${packages.ffmpeg-bulk}/bin/ffmpeg-bulk *.$in -t $out
	${pkgs.coreutils}/bin/rm *.$in
	
	#Remove metadata so the glowies don't see it.
	${pkgs.python312Packages.mat2}/bin/mat2 *.$out
	
	#Transfer
	${pkgs.coreutils}/bin/mv ~/*.cleaned.$out /run/media/$USER/$device/Podcasts
	${pkgs.coreutils}/bin/rm ~/*.$out
      '');
      ffmpeg-bulk = (pkgs.writeShellScriptBin "ffmpeg-bulk" ''
	
      '');
      masstube = pkgs.writeShellScriptBin "masstube" ''
	emacsclient ~/.download.txt
	# ${pkgs.yt-dlp}/bin/yt-dlp $1 --verbose -ci --batch-file=~/.download.txt --proxy socks://localhost:9050
	${pkgs.yt-dlp}/bin/yt-dlp $1 --verbose -ci --batch-file=~/.download.txt 
	${pkgs.coreutils}/bin/rm ~/.download.txt
      '';
    };
  } ;

  flake.homeModules.media = { pkgs, ... }: {
    home.packages = with pkgs; [
      masstube
      cast
    ];
  };
}
