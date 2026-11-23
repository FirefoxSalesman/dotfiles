{ inputs, ... }:
{
  flake = {
    mkEnvVar = pkgs: config: secretName: varName: {
      sops.secrets."${secretName}" = { };
      home.sessionVariables."${varName}" = "$(${pkgs.coreutils}/bin/cat ${
        config.sops.secrets."${secretName}".path
      })";
    };

    homeModules.sops = { pkgs, ... }: {
      imports = [ inputs.sops-nix.homeManagerModules.sops ];
      home.packages = [ pkgs.sops ];
      sops = {
        age.keyFile = "/home/holschcc/.config/sops/age/keys.txt";
        defaultSopsFile = ../secrets/secrets.yaml;
        defaultSopsFormat = "yaml";
      };
      programs.emacs.init.usePackage.sops = {
        enable = true;
        hook = [
          {
            names = [ "on-first-file" ];
            actions = [ "'global-sops-mode" ];
          }
        ];
      };
    };
  };
}
