{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{

  config = lib.mkIf ide.languages.racket.enable {
  };
}
