{ pkgs, ... }:

{
  programs.emacs.init.usePackage.erlang-ts = {
    enable = true;
    mode = [''("\\.erl\\'" . erlang-ts-mode)''];
    eglot = true;
    symex = true;
  };
}
