{ inputs, ... } :
{
  perSystem = { pkgs, self', ... }: let epkgs = pkgs.emacs.pkgs;
  in {
    packages = {
      emacs-ezf = (epkgs.callPackage
	epkgs.trivialBuild rec {
	  pname = "ezf";
	  version = "current";
	  src = inputs.ezf;
	}
      );
      ezf = pkgs.writeShellScriptBin "ezf" ''
	set -o nounset -o errexit -o pipefail
	
	field=0-
	# the elisp function to use for completing read
	candidate_fn=ezf-default
	while getopts c:f: OPT; do
	    case $OPT in
	        c)
	            candidate_fn=$OPTARG
	            ;;
	        f)
	            field=$OPTARG
	            ;;
	        ,*)
	            ${pkgs.coreutils}/bin/echo "usage: ezf [-f field] [-c candidate-fn]"
	            exit 2
	    esac
	done
	shift $(( OPTIND - 1 ))
	OPTIND=1
	
	ezftmp="$(mktemp)"
	trap 'rm -f -- "$ezftmp"' EXIT
	> "$ezftmp" cat -
	# xargs is there to strip the "" from the beginning and end of the
	# output from Emacs.  Ensure "field" is always passed as a string by
	# quoting it.
	selection=$(emacsclient -e "(progn (require 'ezf) (ezf \"$ezftmp\" \"$field\" #'$candidate_fn))" | xargs)
	if [[ "$selection" == "nil" ]]; then
	    exit 1
	else
	   ${pkgs.coreutils}/bin/echo "$selection"
	fi
      '';
    };
  };

  flake.homeModules.emacs = { ... }:

  {
    programs.emacs.init.usePackage = {
      ezf = {
	enable = true;
	deferIncrementally = true;
      };
    };
  };
}
