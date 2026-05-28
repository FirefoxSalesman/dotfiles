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
  imports = [inputs.home-manager.flakeModules.home-manager];
  systems = ["x86_64-linux"];
  flake = {
    homeConfigurations."holschcc@emacs" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
	inputs.self.homeModules.main
	inputs.self.homeModules.emacs-host
      ];
    };
    homeConfigurations."holschcc@monitors" = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
	inputs.self.homeModules.main
	inputs.self.homeModules.monitors-host
      ];
    };
    homeModules.main = { lib, config, ... }:

    {
      imports = with inputs; [
	self.homeModules.aesthetics
	self.homeModules.ai
	self.homeModules.canary
	self.homeModules.chat
	self.homeModules.dashTemplate
	self.homeModules.development
	self.homeModules.emacs
	self.homeModules.elisp
	self.homeModules.extraPackages
	self.homeModules.exwm
	self.homeModules.fileManager
	self.homeModules.gaming
	self.homeModules.glx
	self.homeModules.java
	self.homeModules.ledger
	self.homeModules.media
	self.homeModules.notifications
	self.homeModules.passwordManagement
	self.homeModules.pkg
	self.homeModules.sage
	self.homeModules.shellConfig
	self.homeModules.startx
	self.homeModules.web
	self.homeModules.webdev
	self.homeModules.writing
	stylix.homeModules.stylix
	emacs-init.homeModules.emacs-init
	emacs-init.homeModules.emacs-presets
        nur.modules.homeManager.default
      ];

      options.hosts = {
	xrandr-command = lib.mkOption {
	  type = lib.types.str;
	  default = "";
	  description = "The xrandr command (or commands) to execute when running startx.";
	};
	exwm-monitors = lib.mkOption {
	  type = lib.types.listOf lib.types.str;
	  default = "";
	  description = "The value of exwm-randr-workspace-monitor-plist to use for the current host";
	};
	wantBattery = lib.mkOption {
	  type = lib.types.bool;
	  default = false;
	  description = "Enables the battery block in the status bar";
	};
      } ;

      config = {
	home = {
	  username = "holschcc";
	  homeDirectory = "/home/${config.home.username}";

	  # This value determines the Home Manager release that your configuration is
	  # compatible with. This helps avoid breakage when a new Home Manager release
	  # introduces backwards incompatible changes.
	  #
	  # You should not change this value, even if you update Home Manager. If you do
	  # want to update the value, then make sure to first check the Home Manager
	  # release notes.
	  stateVersion = "23.05"; # Please read the comment before changing.
	};
	# Let Home Manager install and manage itself.
	programs.home-manager.enable = true;

	nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
      };
    };
  };
}
