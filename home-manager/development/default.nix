{ pkgs, lib, ... }:

{
  imports = [
    ./python.nix
    ./java.nix
    ./nix.nix
    ./web-development.nix
    ./json.nix
    ./toml.nix
    ./haskell.nix
    ./c.nix
    ./bash.nix
    ./r.nix
    ./prolog.nix
    ./zenscript.nix
    ./rust.nix
    ./lua.nix
    ./plantuml.nix
    ./erlang.nix
    ./sql.nix
    ./forth.nix
    ./go.nix
    ./markdown.nix
    ./zig.nix
    ./latex.nix
    ./csharp.nix
    ./ruby.nix
    ./common-lisp.nix
    ./scheme.nix
  ];

  options = {
    programs.emacs.init.ide = {
      symex = lib.mkEnableOption "enables symex support in all languages that support it";
      lsp.enable = lib.mkEnableOption "enables lsp-mode support in all languages that support it";
      eglot.enable = lib.mkEnableOption "enables eglot support in all languages that support it";
    };
  };
}
