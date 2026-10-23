{
  flake.homeModules.development =
    { pkgs, ... }:

    {
      programs.emacs.init = {
        ide = {
          citre.enable = true;
          dape.enable = true;
          direnv = true;
          treesit-fold.enable = true;
          languages = {
            bash.enable = true;
            json.enable = true;
            lua.enable = true;
            toml.enable = true;
            yaml.enable = true;
            xml.enable = true;
            zenscript.enable = true;
          };
        };

        # Yoinked from heks-emacs
        completions.tempel.templates.lisp-mode.lambda = ''"(lambda (" p ")" n> r> ")"'';

        tools.apheleia = {
          enable = true;
          autoFormat = true;
        };

        usePackage = {
	  eat.init = ''
	    (defun efs/eat-docker-command (container) (require 'eat) (eat (concat "docker exec -it '" container "' bash")))
	    (defun efs/enter-excelsior () (interactive) (efs/eat-docker-command "excelsior-sw-environ"))
	    (defun efs/enter-sdk () (interactive) (efs/eat-docker-command "tetramem-sdk"))
	  '';
          rainbow-delimiters = {
            enable = true;
            ghookf = [ "('prog-mode 'rainbow-delimiters-mode)" ];
          };

          racket-mode.gfhookf = [ "('racket-mode 'hs-minor-mode)" ];

          toml-ts-mode = {
            extraPackages = [ pkgs.tombi ];
            eglot = ''("tombi" "lsp")'';
          };

          popper.setopt.popper-reference-buffers = [ "'dape-repl-mode" ];
        };
      };
    };
}
