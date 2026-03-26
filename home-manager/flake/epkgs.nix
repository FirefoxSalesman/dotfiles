{ inputs, ... }:

{
  perSystem = { pkgs, ... }:
  let epkgs = pkgs.emacs.pkgs;
  in {
    # emacs packages
    packages.qutebrowser-emacs = (epkgs.callPackage ../packages/emacs/qutebrowser.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild dash consult exwm password-store evil;
    });
    packages.doom-nano-modeline = (epkgs.callPackage ../packages/emacs/doom-nano-modeline.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild doom-themes;
    });
    packages.dired-single = (epkgs.callPackage ../packages/emacs/dired-single.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.repeaters = (epkgs.callPackage ../packages/emacs/repeaters.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.app-launcher = (epkgs.callPackage ../packages/emacs/app-launcher.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.emacs-ezf = (epkgs.callPackage ../packages/emacs/ezf.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.gptel-quick = (epkgs.callPackage ../packages/emacs/gptel-quick.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild gptel;
    });
    packages.macher = (epkgs.callPackage ../packages/emacs/macher.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild gptel;
    });
    packages.mpc-wrapper = (epkgs.callPackage ../packages/emacs/mpc-wrapper.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.semel = (epkgs.callPackage ../packages/emacs/semel.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild;
    });
    packages.embark = epkgs.callPackage (
      {
        org,
        consult,
        avy,
        compat,
        elpaBuild,
        fetchurl,
        lib,
      }:
      elpaBuild {
        pname = "embark";
        ename = "embark";
        version = "1.1";
        src = fetchurl {
	  url = "https://elpa.gnu.org/packages/embark-1.1.tar";
	  sha256 = "074ggh7dkr5jdkwcndl6znhkq48jmc62rp7mc6vjidr6yxf8d1rn";
        };
        packageRequires = [
	  org
	  consult
	  avy
	  compat
        ];
        meta = {
	  homepage = "https://elpa.gnu.org/packages/embark.html";
	  license = lib.licenses.free;
        };
      }
    ) { };
    packages.reka = ../packages/emacs/reka.nix;
  };
}
