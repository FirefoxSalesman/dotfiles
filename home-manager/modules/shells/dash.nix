{
  flake.homeModules.shellConfig = { pkgs, lib, ... }:
  {
    programs.dash = {
      enable = true;
      initExtra = ''
        ${lib.getExe pkgs.pfetch}
        . ~/.cache/wal/colors.sh
      '';
      shellAliases.z = "cd ./$(ls -d */ .*/ | ${lib.getExe pkgs.ezf})";
      profileExtra = ''
	export LEIN_HOME="$XDG_DATA_HOME/lein";
	export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc";
	export XCOMPOSECACHE="$XDG_CACHE_HOME/x11/xcompose";
	export GOPATH="$XDG_DATA_HOME/go";
	export DOT_SAGE="$XDG_CONFIG_HOME/sage";
	# export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority";
	export CARGO_HOME="$XDG_DATA_HOME/cargo";
	export GNUPGHOME="$XDG_DATA_HOME/share/gnupg";
	export PASSWORD_STORE_DIR="$XDG_DATA_HOME/share/pass";
	export CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv";
	export ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"
	export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
	export HISTFILE="''${XDG_STATE_HOME}"/bash/history
	GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle
	
	[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx
      '';
    };
  };
}
