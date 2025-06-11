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
          inputs.emacs-overlay.overlay
          (final: prev: {
            ezf = (import ./scripts/ezf.nix { inherit pkgs; });
            cast = (import ./scripts/cast.nix { inherit pkgs; });
            doomer = (import ./scripts/doomer.nix { inherit pkgs; });
            ffmpeg-bulk = (import ./scripts/ffmpeg-bulk.nix { inherit pkgs; });
            masstube = (import ./scripts/masstube.nix { inherit pkgs; });
            pkg = (import ./scripts/pkg.nix { inherit pkgs; });
            udisksmenu = (import ./scripts/udisksmenu.nix { inherit pkgs; });
            wiki = (import ./scripts/wiki.nix { inherit pkgs; });
            emacsPackagesFor = emacs: (
              (prev.emacsPackagesFor emacs).overrideScope (
                nfinal: nprev: {
                  qutebrowser = (prev.emacsPackages.callPackage ./packages/emacs/qutebrowser.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild dash consult exwm password-store evil;
                  });
                  doom-nano-modeline = (prev.emacsPackages.callPackage ./packages/emacs/doom-nano-modeline.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild doom-themes;
                  });
                  treesitter-context = (prev.emacsPackages.callPackage ./packages/emacs/treesitter-context.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild posframe;
                  });
                  dired-single = (prev.emacsPackages.callPackage ./packages/emacs/dired-single.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  repeaters = (prev.emacsPackages.callPackage ./packages/emacs/repeaters.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  doom-utils = (prev.emacsPackages.callPackage ./packages/emacs/doom-utils.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  app-launcher = (prev.emacsPackages.callPackage ./packages/emacs/app-launcher.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  ezf = (prev.emacsPackages.callPackage ./packages/emacs/ezf.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  org-modern-indent = (prev.emacsPackages.callPackage ./packages/emacs/org-modern-indent.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild compat;
                  });
                  exwm-outer-gaps = (prev.emacsPackages.callPackage ./packages/emacs/exwm-outer-gaps.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild exwm xelb;
                  });
                  gptel-quick = (prev.emacsPackages.callPackage ./packages/emacs/gptel-quick.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild gptel;
                  });
                  mpc-wrapper = (prev.emacsPackages.callPackage ./packages/emacs/mpc-wrapper.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild;
                  });
                  symex = (prev.emacsPackages.callPackage ./packages/emacs/symex2.nix {
                    inherit inputs;
                    inherit (prev.emacsPackages) trivialBuild tsc tree-sitter evil evil-surround seq paredit;
                  });
                  embark = prev.emacsPackages.callPackage (
                    {
                      org,
                      consult,
                      avy,
                      compat,
                      elpaBuild,
                      fetchurl,
                      lib,
                    }:
                    elpaBuild {
                      pname = "embark";
                      ename = "embark";
                      version = "1.1";
                      src = fetchurl {
                        url = "https://elpa.gnu.org/packages/embark-1.1.tar";
                        sha256 = "074ggh7dkr5jdkwcndl6znhkq48jmc62rp7mc6vjidr6yxf8d1rn";
                      };
                      packageRequires = [
                        org
                        consult
                        avy
                        compat
                      ];
                      meta = {
                        homepage = "https://elpa.gnu.org/packages/embark.html";
                        license = lib.licenses.free;
                      };
                    }
                  ) { };
                }));
          })];
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
            inherit (inputs)  apple-fonts doom-utils repeaters ezf dired-single launcher doom-nano-modeline symex2 treesitter-context gptel-quick eglot-x org-modern-indent mpc-wrapper exwm-qutebrowser exwm-outer-gaps;
            inherit pkgs-stable;
          };
          modules = [
            ./home.nix
            stylix.homeModules.stylix
            emacs-init.homeModules.emacs-init
          ];
        } ;
      };
}
