{ pkgs, ... }:

{
  programs.emacs.init.usePackage.racket-mode = {
    enable = true;
    eglot = true;
    symex = true;
    mode = [''"\\.rkt\\'"''];
    gfhook = ["('racket-mode-hook 'hs-minor-mode)"];
    init = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
    config = ''(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist))'';
    generalTwo.local-leader.racket-mode-map = {
      "RET" = "'geiser-racket";
      "." = "'racket-xp-describe";
      "r" = "'racket-run";
    };
  };
}
