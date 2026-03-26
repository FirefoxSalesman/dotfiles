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
  flake = {
    homeConfigurations.holschcc = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {
	inherit inputs;
      };
      modules = [
	inputs.self.homeModules.aesthetics
	inputs.self.homeModules.ai
	inputs.self.homeModules.basic
	inputs.self.homeModules.canary
	inputs.self.homeModules.dashTemplate
	inputs.self.homeModules.development
	inputs.self.homeModules.emacs
	inputs.self.homeModules.extraPackages
	inputs.self.homeModules.exwm
	inputs.self.homeModules.fileManager
	inputs.self.homeModules.media
	inputs.self.homeModules.passwordManagement
	inputs.self.homeModules.sage
	inputs.self.homeModules.shellConfig
	inputs.self.homeModules.startx
	inputs.self.homeModules.web
	inputs.self.homeModules.writing
	inputs.stylix.homeModules.stylix
	inputs.emacs-init.homeModules.emacs-init
	inputs.emacs-init.homeModules.emacs-presets
      ];
    };
    homeModules.basic = { lib, config, ... }:

    {
      # Home Manager needs a bit of information about you and the paths it should
      # manage.
      home.username = "holschcc";
      home.homeDirectory = "/home/holschcc";

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

      targets.genericLinux.nixGL.packages = inputs.nixgl.packages;
      
      targets.genericLinux.enable = true;
      
      home.activation = {
        clearNixglCache = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            [ -v DRY_RUN ] || rm -f ${config.xdg.cacheHome}/nixgl/result*
          '';
      };

      home.file.".alsoftrc".text = ''
        [general]
        drivers=pulse
        hrtf=true
      '';

      xdg.userDirs = {
          enable = true;
          createDirectories = true;
          desktop = null;
          publicShare = null;
          templates = null;
          documents = "${config.home.homeDirectory}/doc";
          download = "${config.home.homeDirectory}/dwn";
          music = "${config.home.homeDirectory}/mus";
          pictures = "${config.home.homeDirectory}/pic";
          videos = "${config.home.homeDirectory}/vid";
      };
    };
  };
}
