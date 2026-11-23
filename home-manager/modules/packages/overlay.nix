{ inputs, self, ... }:

let
  system = "x86_64-linux";
in
{
  flake = {
    pkgs-stable = import inputs.nixpkgs-stable let {
      inherit system;
      config = {
        allowUnfree = true;
        nvidia.acceptLicense = true;
        permittedInsecurePackages = [
          "qutebrowser-3.7.0"
        ];
      };
    };

    overlay =
      final: prev: pkgs: inputs:
      let
        packages = self.packages.${pkgs.system};
      in
      {
        emacsPackagesFor =
          emacs:
          ((prev.emacsPackagesFor emacs).overrideScope (
            nfinal: nprev: {
              qutebrowser = packages.qutebrowser-emacs;
              doom-nano-modeline = packages.doom-nano-modeline;
              repeaters = packages.repeaters;
              app-launcher = packages.app-launcher;
              ezf = packages.emacs-ezf;
              mpc-wrapper = packages.mpc-wrapper;
              roll = packages.roll;
              pertab = packages.pertab;
              embark = packages.embark;
              code-review = packages.code-review;
              whisper = packages.whisper;
              gptel-preset-collection = packages.gptel-preset-collection;
            }
          ));
      };
  };
}
