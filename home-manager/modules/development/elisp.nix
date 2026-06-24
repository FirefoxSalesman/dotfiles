{
  flake.homeModules.elisp =
    { ... }:
    {
      programs.emacs.init = {
        completions.tempel.templates.emacs-lisp-mode = {
          wcd = ''"(with-current-buffer " "q)"'';
          gbc = ''"(get-buffer-create " "q)"'';
        };
        ide.languages.emacs-lisp = {
          enable = true;
          semel = true;
        };
        tools.apheleia = {
          formatters.elisp-autofmt = ''
            (make-apheleia-formatter apheleia-indent-elisp
               (elisp-autofmt-buffer))
          '';
	  modeFormatters.emacs-lisp-mode = "elisp-autofmt";
        };
        usePackage = {
          elisp-autofmt = {
            enable = true;
            after = [ "apheleia" ];
          };
          elisp-mode.gfhookf = [
            "('emacs-lisp-mode (local! completion-at-point-functions (list (cape-capf-super 'tempel-complete 'elisp-completion-at-point))))"
          ];
        };
      };
    };
}
