{ pkgs, lib, ... }:

{
  imports = [
    ./clojure.nix
    ./python.nix
    ./java.nix
    ./nix.nix
    ./web-development.nix
    ./json.nix
    ./toml.nix
    ./racket.nix
    # ./haskell.nix
    ./c.nix
    ./bash.nix
    ./r.nix
    ./prolog.nix
    ./zenscript.nix
    # ./rust.nix
    ./lua.nix
    ./plantuml.nix
    ./scala.nix
    ./erlang.nix
    ./sql.nix
    ./forth.nix
  ];

  options = {
    programs.emacs.init.ide = {
      symex = lib.mkEnableOption "enables symex support in all languages that support it";
      lsp = lib.mkEnableOption "enables lsp-mode support in all languages that support it";
      eglot = lib.mkEnableOption "enables eglot support in all languages that support it";
    };
  };
}
