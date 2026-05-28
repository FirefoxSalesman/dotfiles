{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    emacs-overlay.url  = "github:nix-community/emacs-overlay/1746faa";
    # emacs-overlay.url  = "github:nix-community/emacs-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";

    wrapper-modules.url = "github:BirdeeHub/nix-wrapper-modules";

    nur.url = "github:nix-community/NUR";
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
      url = "github:kmonad/kmonad?dir=nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # packages
    repeaters = {
      url = "github:mmarshall540/repeaters";
      flake = false;
    };

    ezf = {
      url = "github:firefoxsalesman/ezf";
      flake = false;
    };

    app-launcher = {
      url = "github:SebastienWae/app-launcher";
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
      # url = "github:lrustand/qutebrowser.el/18f98f0";
      url = "github:lrustand/qutebrowser.el";
      flake = false;
    };

    pertab = {
      url = "github:firefoxsalesman/pertab";
      flake = false;
    };

    roll = {
      url = "github:finalclass/emacs-roll";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake {inherit inputs;} (inputs.import-tree ./modules);
}
