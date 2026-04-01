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
  imports = [
    inputs.home-manager.flakeModules.home-manager
    inputs.flake-file.flakeModules.dendritic 
  ];
  systems = ["x86_64-linux"];
  flake = {
    homeConfigurations.holschcc = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
	inputs.self.homeModules.aesthetics
	inputs.self.homeModules.ai
	inputs.self.homeModules.basic
	inputs.self.homeModules.canary
	inputs.self.homeModules.chat
	inputs.self.homeModules.dashTemplate
	inputs.self.homeModules.development
	inputs.self.homeModules.emacs
	inputs.self.homeModules.elisp
	inputs.self.homeModules.extraPackages
	inputs.self.homeModules.exwm
	inputs.self.homeModules.fileManager
	inputs.self.homeModules.gaming
	inputs.self.homeModules.glx
	inputs.self.homeModules.java
	inputs.self.homeModules.ledger
	inputs.self.homeModules.media
	inputs.self.homeModules.notifications
	inputs.self.homeModules.passwordManagement
	inputs.self.homeModules.sage
	inputs.self.homeModules.shellConfig
	inputs.self.homeModules.startx
	inputs.self.homeModules.web
	inputs.self.homeModules.webdev
	inputs.self.homeModules.writing
	inputs.stylix.homeModules.stylix
	inputs.emacs-init.homeModules.emacs-init
	inputs.emacs-init.homeModules.emacs-presets
      ];
    };
    homeModules.basic = { lib, config, ... }:

    {
      home.username = "holschcc";

      # This value determines the Home Manager release that your configuration is
      # compatible with. This helps avoid breakage when a new Home Manager release
      # introduces backwards incompatible changes.
      #
      # You should not change this value, even if you update Home Manager. If you do
      # want to update the value, then make sure to first check the Home Manager
      # release notes.
      home.stateVersion = "23.05"; # Please read the comment before changing.

      # Let Home Manager install and manage itself.
      programs.home-manager.enable = true;

      nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    };
  };
}
