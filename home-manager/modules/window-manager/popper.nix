{
  flake.homeModules.exwm = { ... }:
  {
    programs.emacs.init.usePackage.popper = {
      enable = true;
      ghookf = ["('on-first-buffer 'popper-mode)"];
      general = {
	"s-'" = "'popper-toggle";
	"s-\\\"" = "'popper-cycle";
	"C-s-'" = "'popper-toggle-type";
      };
      setopt = {
	popper-window-height = 30;
	popper-group-function = "'popper-group-by-project";
	popper-reference-buffers = [
	  "'help-mode"
	  "'helpful-mode"
	  "'compilation-mode"
	  "'inferior-python-mode"
	  "'occur-mode"
	  "'grep-mode"
	  ''"^\\*.*eshell\\*"''
	  ''"^\\*eat\\*"''
	  ''"^\\*Sage\\*"''
	  ''"^\\*prolog\\*"''
	  "'xref--xref-buffer-mode"
	  "'flymake-diagnostics-buffer-mode"
	  "'rustic-cargo-test-mode"
	  "'rustic-cargo-run-mode"
	  "'geiser-repl-mode"
	  "'dape-repl-mode"
	  "'racket-repl-mode"
	  "'inferior-ess-r-mode"
	  "'cider-repl-mode"
	];
      };
    };
  };
}
