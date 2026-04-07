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
	#===============================================================================
	#
	#		  USAGE: ./this.sh --help
	# 
	#	DESCRIPTION: Create a ffmpeg conversion script from a list of input files.
	# 
	#		OPTIONS: ---
	#  REQUIREMENTS: sed, gawk, ffmpeg, tee
	#		   BUGS: ---
	#		  NOTES: ---
	#		 AUTHOR: Sylvain Saubier (ResponSyS), mail@sylsau.com
	#		CREATED: 01/05/16 14:09
	#===============================================================================
	
	[[ $DEBUG ]] && set -o nounset
	set -o pipefail -o errexit -o errtrace
	trap 'echo -e "''${FMT_BOLD}ERROR''${FMT_OFF}: at $FUNCNAME:$LINENO"' ERR
	
	readonly FMT_BOLD='\e[1m'
	readonly FMT_UNDERL='\e[4m'
	readonly FMT_OFF='\e[0m'
	
	readonly PROGRAM_NAME="''${0##*/}"
	readonly SCRIPT_NAME="''${0##*/}"
	RES="$( stat -c %y $0 | cut -d" " -f1 )"
	readonly VERSION=''${RES//-/}
	
	readonly ERR_NO_CMD=60
	
	FFMPEG="''${FFMPEG:-ffmpeg}"
	OPT_EXT=
	OPT_ARGS_IN=
	OPT_ARGS_OUT=
	OPT_FORCE=
	OPT_LOGLEVEL="-loglevel error"
	INPUT=( )
	
	
	# $1 = command to test (string)
	fn_need_cmd() {
		if ! command -v "$1" > /dev/null 2>&1
			then fn_err "need '$1' (command not found)" $ERR_NO_CMD
		fi
	}
	# $1 = message (string)
	m_say() {
		echo -e "$PROGRAM_NAME: $1"
	}
	# $1 = error message (string), $2 = return code (int)
	fn_err() {
		m_say "''${FMT_BOLD}ERROR''${FMT_OFF}: $1" >&2
		exit $2
	}
	
	fn_show_params() {
		m_say "\n input=''${INPUT[*]}\n -t=$OPT_EXT\n -ai=$OPT_ARGS_IN\n -ao=$OPT_ARGS_OUT\n -f=$OPT_FORCE\n -q=$OPT_LOGLEVEL" >&2
	}
	
	
	fn_need_cmd "$FFMPEG"
	
	# Check args
	if [[ -z "$@" ]]; then
		exit
	else
		while [[ $# -gt 0 ]]; do
			case "$1" in
				"--to"|"-t")
					OPT_EXT=$2
					shift
					;;
				"--args-in"|"-ai")
					OPT_ARGS_IN=$2
					shift
					;;
				"--args-out"|"-ao")
					OPT_ARGS_OUT=$2
					shift
					;;
				"--force"|"-f")
					OPT_FORCE="-y"
					;;
				"--log-level")
					OPT_LOGLEVEL="-loglevel $2"
					shift
					;;
				*)
					[[ -e "$1" ]] || fn_err "file '$1' does not exist" 127
					INPUT+=( "$1" )
					;;
			esac	# --- end of case ---
			shift 	# delete $1
		done
	fi
	
	[[ $DEBUG ]] && fn_show_params
	
	[[ $OPT_EXT ]] || fn_err "please specify the output extension with -t EXT" 2
	
	# Rajoute un point à l'extension si absent
	if [[ ''${OPT_EXT:0:1} != '.' ]]; then
		OPT_EXT=.$OPT_EXT
	fi
	
	m_say "converting...\n---"
	for F in "''${INPUT[@]}"; do # Just show the commands
		${pkgs.coreutils}/bin/echo $FFMPEG $OPT_ARGS_IN -i "$F" $OPT_ARGS_OUT $OPT_FORCE $OPT_LOGLEVEL "''${F%.*}$OPT_EXT"
	done ; ${pkgs.coreutils}/bin/echo "---" ; [[ $DEBUG ]] && exit
	for F in "''${INPUT[@]}"; do # Actually execute
		m_say "converting \"$F\"..."
		     $FFMPEG $OPT_ARGS_IN -i "$F" $OPT_ARGS_OUT $OPT_FORCE $OPT_LOGLEVEL "''${F%.*}$OPT_EXT"
	done
	
	exit
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
