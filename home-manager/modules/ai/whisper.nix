{ inputs, ... }:

{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.whisper = (
        epkgs.callPackage epkgs.trivialBuild rec {
          pname = "whisper";
          version = "current";
          src = inputs.whisper;

          propagatedUserEnvPkgs = with epkgs; [ ];

          buildInputs = propagatedUserEnvPkgs;
        }
      );
    };

  flake.homeModules.ai = { pkgs, ... }: {
    programs.emacs.init.usePackage.whisper = {
      enable = true;
      extraPackages = [ pkgs.cmake ];
      generalOne.global-leader."W" = "'whisper-run";
      setopt.whisper-install-directory = ''(concat user-emacs-directory "/whisper")'';
    };
  };
}
