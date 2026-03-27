{
  flake.homeModules.development = { ... }:
  {
    programs.emacs.init = {
      completions.tempel.templates.emacs-lisp-mode = {
	wcd = ''"(with-current-buffer " "q)"'';
	gbc = ''"(get-buffer-create " "q)"'';
      };
      ide.languages.emacs-lisp.enable = true;
      usePackage = {
	elisp-mode.gfhookf = ["('emacs-lisp-mode (local! completion-at-point-functions (list (cape-capf-super 'tempel-complete 'elisp-completion-at-point))))"];
	
	semel = {
	  enable = true;
	  ghookf = ["('emacs-lisp-mode 'semel-mode)"];
	};
      };
    };
  };
}
