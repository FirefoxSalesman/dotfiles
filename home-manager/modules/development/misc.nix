{
  flake.homeModules.development =
    { ... }:

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
            nix.enable = true;
            toml.enable = true;
            xml.enable = true;
            zenscript.enable = true;
            yaml.enable = true;
          };
        };

        tools.apheleia = {
          enable = true;
          autoFormat = true;
        };

        usePackage = {
          rainbow-delimiters = {
            enable = true;
            ghookf = [ "('prog-mode 'rainbow-delimiters-mode)" ];
          };

          racket-mode.gfhookf = [ "('racket-mode 'hs-minor-mode)" ];
        };
      };
    };
}
