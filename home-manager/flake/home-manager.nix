{ self, inputs, ... }:
let
  system = "x86_64-linux";
    pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [
      (final: prev: self.overlay final prev pkgs inputs)
      inputs.emacs-init.overlay
      inputs.emacs-overlay.overlay
    ];
  };
in {
  flake.homeConfigurations.holschcc = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit inputs;
    };
    modules = [
      ../home.nix
      inputs.stylix.homeModules.stylix
      inputs.emacs-init.homeModules.emacs-init
      inputs.emacs-init.homeModules.emacs-presets
    ];
  };
}
