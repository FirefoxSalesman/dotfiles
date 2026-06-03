{
  flake.homeModules.emacs = { ... }: {
    programs.emacs = {
      extraPackages = epkgs: [ epkgs.pretty-hydra ];
      init.prelude = ''
	(use-package pretty-hydra
	  :demand t
	  :custom
	  (hydra-hint-display-type 'posframe)
	  :config
	  (gsetq hydra-posframe-show-params '(:internal-border-width 1
								     :internal-border-color "003f28"
								     :poshandler posframe-poshandler-frame-bottom-center
								     :refposhandler vertico-posframe-refposhandler-default))
	  :gfhookf ('doom-escape 'hydra-keyboard-quit))
      '';
    };
  };
}
