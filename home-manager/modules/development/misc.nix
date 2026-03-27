{
  flake.homeModules.development = { ... }:

  {
    programs.emacs.init = {
      ide = {
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

      usePackage = {
	rainbow-delimiters = {
	  enable = true;
	  ghookf = ["('prog-mode 'rainbow-delimiters-mode)"];
	};
      
	racket-mode.gfhookf = ["('racket-mode 'hs-minor-mode)"];
      };
    };
  };
}
