{ inputs, self, ... }:
let
  system = "x86_64-linux";
  
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [
      (final: prev: import ./overlay.nix final prev pkgs inputs)
      inputs.emacs-init.overlay
      inputs.emacs-overlay.overlay
    ];
  };
  pkgs-stable = import inputs.nixpkgs-stable {
    inherit system;
    config.allowUnfree = true;
  };
in {
  flake.homeConfigurations.holschcc = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit self;
      inherit inputs;
      inherit system;
      inherit pkgs-stable;
    };
    modules = [
      ../home.nix
      inputs.stylix.homeModules.stylix
      inputs.emacs-init.homeModules.emacs-init
      inputs.emacs-init.homeModules.emacs-presets
    ];
  };
}
