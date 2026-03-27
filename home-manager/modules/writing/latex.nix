{
  flake.homeModules.writing = { pkgs, ... }:
  {
    home.packages = with pkgs; [ texlive.combined.scheme-full ];
    programs.emacs.init = {
      ide.languages.latex = {
	enable = true;
	magicLatexBuffer = true;
	cdlatex = true;
      };
      usePackage = {
	cdlatex.generalTwoConfig.":i" = {
	  cdlatex-mode-map."TAB" = "'cdlatex-tab";
	  org-cdlatex-mode-map."TAB" = "'cdlatex-tab";
	};

	tex.generalTwoConfig.local-leader.LaTeX-mode-map."e" = "'TeX-command-run-all";

	evil-tex.generalOneConfig.":ov" = {
	  "t" = "evil-tex-outer-text-objects-map";
	  "s" = "evil-tex-inner-text-objects-map";
	};
      };
    };
  };
}
