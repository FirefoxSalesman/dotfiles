{ pkgs, lib, config, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.racket.enable = lib.mkEnableOption "enables racket support";

  config = lib.mkIf ide.languages.racket.enable {
    programs.emacs.init.usePackage.racket-mode = {
      enable = true;
      eglot = ide.eglot;
      lsp = ide.lsp;
      symex = ide.symex;
      mode = [''"\\.rkt\\'"''];
      init = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
      config = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
    };
  };
}
