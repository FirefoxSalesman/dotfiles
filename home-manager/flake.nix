{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    emacs-overlay.url  = "github:nix-community/emacs-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    
    stylix.url = "github:danth/stylix";
    
    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";
    
    emacs-init.url = "github:firefoxsalesman/NixEmacs";
    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    nixgl = {
      url = "github:nix-community/nixGL"; 
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    kmonad = {
      url = "github:kmonad/kmonad?submodules=1&dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # packages
    gptel-quick = {
      url = "github:karthink/gptel-quick";
      flake = false;
    };
    
    repeaters = {
      url = "github:mmarshall540/repeaters";
      flake = false;
    };
    
    app-launcher = {
      url = "github:SebastienWae/app-launcher";
      flake = false;
    };
    
    emacs-embark = {
      url = "github:oantolin/embark";
      flake = false;
    };
    
    dired-single = {
      url = "github:firefoxsalesman/dired-single";
      flake = false;
    };
    
    ezf = {
      url = "github:firefoxsalesman/ezf";
      flake = false;
    };
    
    doom-nano-modeline = {
      url = "github:ronisbr/doom-nano-modeline";
      flake = false;
    };
    
    mpc-wrapper = {
      url = "github:FirefoxSalesman/mpc-wrapper";
      flake = false;
    };
    
    exwm-qutebrowser = {
      url = "github:lrustand/qutebrowser.el/18f98f0";
      flake = false;
    };
    
    
    semel = {
      url = "github:eshelyaron/semel";
      flake = false;
    };
  };

  outputs = inputs@{flake-parts, nixgl, self, ... }:
  flake-parts.lib.mkFlake { inherit inputs; } (top@{ config, withSystem, moduleWithSystem, ... }: {
    imports = [
      inputs.home-manager.flakeModules.home-manager
      ./modules/packages
      ./modules/emacs.nix
      ./modules/emacs/completion-system.nix
      ./modules/emacs/early-init.nix
      ./modules/emacs/evil.nix
      ./modules/emacs/help-system.nix
      ./modules/emacs/writing.nix
      ./modules/aesthetics.nix
      ./modules/dash.nix
      ./modules/development.nix
      ./modules/extra-packages.nix
      ./modules/file-management.nix
      ./modules/gptel.nix
      ./modules/gui.nix
      ./modules/home.nix
      ./modules/internet.nix
      ./modules/keyboard.nix
      ./modules/multimedia.nix
      ./modules/passwords.nix
      ./modules/sage.nix
      ./modules/shells.nix
      ./modules/window-manager.nix
    ];
    flake.gpuWrappers = nixgl.defaultPackage;
    systems = ["x86_64-linux"];
  });
}
