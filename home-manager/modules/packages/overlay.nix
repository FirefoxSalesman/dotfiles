{ inputs, self, ... }:

let
  system = "x86_64-linux";
  pkgs-stable = import inputs.nixpkgs-stable {
    inherit system;
    config.allowUnfree = true;
  };
in {
  flake.overlay = final: prev: pkgs: inputs:
  let packages = self.packages.${pkgs.system};
  in {
    # shell scripts
    ezf = packages.ezf;
    cast = packages.cast;
    doomer = packages.doomer;
    ffmpeg-bulk = packages.ffmpeg-bulk;
    masstube = packages.masstube;
    mpvmacs = packages.mpvmacs;
    hdmihelper = packages.hdmihelper;
    i3status-rs = packages.i3status-rs;
    pkg = packages.pkg;
    startOllama = packages.startOllama;
    udisksmenu = packages.udisksmenu;
    updatefix = packages.updatefix;
    wiki = packages.wiki;

    # overrides
    mpv = packages.mpv;

    #emacs packages
    emacsPackagesFor = emacs: (
      (prev.emacsPackagesFor emacs).overrideScope (
	nfinal: nprev: {
          qutebrowser = packages.qutebrowser-emacs;
          doom-nano-modeline = packages.doom-nano-modeline;
          dired-single = packages.dired-single;
          repeaters = packages.repeaters;
          app-launcher = packages.app-launcher;
          ezf = packages.emacs-ezf;
	  mpc-wrapper = packages.mpc-wrapper;
	  semel = packages.semel;
          embark = packages.embark;
	}));
  };
}
