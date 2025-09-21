{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    emacs-overlay.url  = "github:nix-community/emacs-overlay";
    
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
    
    ragmacs = {
      url = "github:positron-solutions/ragmacs";
      flake = false;
    };
    
    macher = {
      url = "github:kmontag/macher";
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
    
    org-novelist = {
      url = "github:sympodius/org-novelist";
      flake = false;
    };
    
    mpc-wrapper = {
      url = "github:FirefoxSalesman/mpc-wrapper";
      flake = false;
    };
    
    exwm-outer-gaps = {
      url = "github:firefoxsalesman/exwm-outer-gaps";
      flake = false;
    };
    
    exwm-qutebrowser = {
      url = "github:lrustand/qutebrowser.el/18f98f0";
      flake = false;
    };
    
  };

  outputs = { self, home-manager, nixgl, nixpkgs, nixpkgs-stable, stylix, emacs-init, ... }@inputs:
    let
      system = "x86_64-linux";
      
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: import ./overlay.nix final prev pkgs inputs)
          inputs.emacs-init.overlay
          inputs.emacs-overlay.overlay
        ];
      };
      pkgs-stable = import nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
      };
    in
      {
        gpuWrappers = nixgl.defaultPackage;
        # gpuWrappers = nixgl.nixGLNvidia;
        homeConfigurations."holschcc" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit self;
            inherit inputs;
            inherit system;
            inherit (inputs)  apple-fonts doom-utils repeaters ezf dired-single launcher doom-nano-modeline gptel-quick org-novelist mpc-wrapper exwm-qutebrowser exwm-outer-gaps;
            inherit pkgs-stable;
          };
          modules = [
            ./home.nix
            stylix.homeModules.stylix
            emacs-init.homeModules.emacs-init
            emacs-init.homeModules.emacs-presets
          ];
        } ;
      };
}
