{ pkgs }:

pkgs.writeShellScriptBin "pkg" ''
  optimize() {
    nix-collect-garbage -d
    nix-store --optimise
    doas pacman -Sc --noconfirm
  }

  update() {
    nix flake update --flake ~/.config/home-manager/
    home-manager switch --flake ~/.config/home-manager/#holschcc
    doas nix flake update --flake /etc/system-manager/
    doas nix run 'github:numtide/system-manager' -- switch --flake '/etc/system-manager/'
    yay -Syu
  }

  help() {
    ${pkgs.coreutils}/bin/echo "help: display this message"
    ${pkgs.coreutils}/bin/echo "install [packages]: install packages with yay"
    ${pkgs.coreutils}/bin/echo "optimize: clean up package cache"
    ${pkgs.coreutils}/bin/echo "update: update all packages"
    ${pkgs.coreutils}/bin/echo "add: install a package"
    ${pkgs.coreutils}/bin/echo "rm: uninstall a package"
    ${pkgs.coreutils}/bin/echo "flake: perform a nix flake operation"
    ${pkgs.coreutils}/bin/echo "template: set up a devshell template"
    ${pkgs.coreutils}/bin/echo "tmp: temporarily install a package"
  }

  getTemplates() {
    ${pkgs.coreutils}/bin/printf "bun\nc-cpp\nclojure\ncsharp\ncue\ndhall\nelixir\nelm\nempty\ngleam\ngo\nhashi\nhaskell\nhaxe\njava\njupyter\nkotlin\nlatex\nnickel\nnim\nnix\nnode\nocaml\nopa\nphp\nplatformio\nprotobuf\npulumi\npurescript\npython\nr\nruby\nrust\nrust-toolchain\nscala\nshell\nswi-prolog\nswift\nvlang\nzig" | ${pkgs.ezf}/bin/ezf
  }

  template() {
    nix flake init --template "https://flakehub.com/f/the-nix-way/dev-templates/*#$(getTemplates)" 
    ${pkgs.direnv}/bin/direnv allow
  }

  case "$1" in
    optimize ) optimize ;;
    update ) update ;;
    add ) yay -S ''${@:2} ;;
    rm ) yay -Rs ''${@:2} ;;
    flake ) nix flake ''${@:2} ;;
    tmp ) nix-shell -p ''${@:2};;
    template ) template;;
    help ) help ;;
  esac
''
