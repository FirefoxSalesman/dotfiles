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
            toml.enable = true;
            yaml.enable = true;
            xml.enable = true;
          };
        };

        # Yoinked from heks-emacs
        completions.tempel.templates.lisp-mode.lambda = ''"(lambda (" p ")" n> r> ")"'';

        tools.apheleia = {
          enable = true;
          autoFormat = true;
        };

        usePackage = {
	  rainbow-delimiters = {
            enable = true;
            hook = [
              {
                names = [ "prog-mode" ];
                actions = [ "'rainbow-delimiters-mode" ];
              }
            ];
          };

          racket-mode.hook = [
            {
              namesDeferred = [ "racket-mode " ];
              actions = [ "'hs-minor-mode" ];
            }
          ];

          toml-ts-mode = {
            extraPackages = [ pkgs.tombi ];
            eglot = ''("tombi" "lsp")'';
          };

          popper.setopt.popper-reference-buffers = [ "'dape-repl-mode" ];
        };
      };
    };
}
