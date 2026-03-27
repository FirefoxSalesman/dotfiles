{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    emacs-overlay.url  = "github:nix-community/emacs-overlay";
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";

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

  outputs = inputs: inputs.flake-parts.lib.mkFlake {inherit inputs;} (inputs.import-tree ./modules);
}
