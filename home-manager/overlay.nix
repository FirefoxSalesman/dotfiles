  final: prev: pkgs: inputs: {
    # shell scripts
    ezf = (import ./scripts/ezf.nix { inherit pkgs; });
    cast = (import ./scripts/cast.nix { inherit pkgs; });
    doomer = (import ./scripts/doomer.nix { inherit pkgs; });
    ffmpeg-bulk = (import ./scripts/ffmpeg-bulk.nix { inherit pkgs; });
    masstube = (import ./scripts/masstube.nix { inherit pkgs; });
    pkg = (import ./scripts/pkg.nix { inherit pkgs; });
    udisksmenu = (import ./scripts/udisksmenu.nix { inherit pkgs; });
    wiki = (import ./scripts/wiki.nix { inherit pkgs; });

    # overrides
    vesktop = (prev.vesktop.override {withSystemVencord = true;});
    mpv = (prev.mpv.override {
      scripts = with prev.mpvScripts; [
        thumbnail
        sponsorblock
      ];
    });

    #emacs packages
    emacsPackagesFor = emacs: (
      (prev.emacsPackagesFor emacs).overrideScope (
        nfinal: nprev: {
          qutebrowser = (prev.emacsPackages.callPackage ./packages/emacs/qutebrowser.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild dash consult exwm password-store evil;
          });
          doom-nano-modeline = (prev.emacsPackages.callPackage ./packages/emacs/doom-nano-modeline.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild doom-themes;
          });
          treesitter-context = (prev.emacsPackages.callPackage ./packages/emacs/treesitter-context.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild posframe;
          });
          dired-single = (prev.emacsPackages.callPackage ./packages/emacs/dired-single.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild;
          });
          repeaters = (prev.emacsPackages.callPackage ./packages/emacs/repeaters.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild;
          });
          app-launcher = (prev.emacsPackages.callPackage ./packages/emacs/app-launcher.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild;
          });
          ezf = (prev.emacsPackages.callPackage ./packages/emacs/ezf.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild;
          });
          org-novelist = (prev.emacsPackages.callPackage ./packages/emacs/org-novelist.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild org;
          });
          exwm-outer-gaps = (prev.emacsPackages.callPackage ./packages/emacs/exwm-outer-gaps.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild exwm xelb;
          });
          gptel-quick = (prev.emacsPackages.callPackage ./packages/emacs/gptel-quick.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild gptel;
          });
          mpc-wrapper = (prev.emacsPackages.callPackage ./packages/emacs/mpc-wrapper.nix {
            inherit inputs;
            inherit (prev.emacsPackages) trivialBuild;
          });
          embark = prev.emacsPackages.callPackage (
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
        }));
  }
