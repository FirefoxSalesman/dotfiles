{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    emacs-overlay.url  = "github:nix-community/emacs-overlay";
    
    stylix.url = "github:danth/stylix";
    
    apple-fonts.url = "github:Lyndeno/apple-fonts.nix";
    
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
    
    # Emacs packages
    gptel-quick = {
      url = "github:karthink/gptel-quick";
      flake = false;
    };
    
    doom-utils = {
      url = "github:firefoxsalesman/doom-utils";
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
    
    org-auto-export-pandoc = {
      url = "github:Y0ngg4n/org-auto-export-pandoc";
      flake = false;
    };
    
    symex2 = {
      url = "github:firefoxsalesman/symex.el/2.0-integration";
      flake = false;
    };
    
    treesitter-context = {
      url = "github:zbelial/treesitter-context.el";
      flake = false;
    };
    
    eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };
    
    eglot-x = {
      url = "github:nemethf/eglot-x";
      flake = false;
    };
    
    org-modern-indent = {
      url = "github:alphapapa/org-modern-indent";
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
      url = "github:lrustand/qutebrowser.el";
      flake = false;
    };
    
    pywal-discord = {
      url = "github:ZephyrCodesStuff/pywal-vencord";
      flake = false;
    };
  };

  outputs = { self, home-manager, nixgl, nixpkgs, nixpkgs-stable, stylix, ... }@inputs:
    let
      system = "x86_64-linux";
      
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
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
            inherit (inputs)  apple-fonts doom-utils repeaters ezf dired-single launcher doom-nano-modeline org-auto-export-pandoc symex2 treesitter-context gptel-quick eglot-x org-modern-indent mpc-wrapper exwm-qutebrowser exwm-outer-gaps pywal-discord; 
            inherit pkgs-stable;
          };
          modules = [
            ./home.nix
            stylix.homeManagerModules.stylix
          ];
        } ;
      };
}
