{
  flake.homeModules.writing = { pkgs, ... }:
  {
    home.packages = with pkgs; [
      hunspell
      hunspellDicts.en-us-large
    ];
    programs.emacs.init = {
      tools = {
	nov = true;
	pdf = true;
      };
      writing.citar = true;
      completions.tempel.templates.org-mode.ci = ''"* Works Cited" n "#+cite_export: csl ~/.config/csl/ieee.csl" n "#+print_bibliography:" q'';
      usePackage = {
	flyspell = {
	  enable = true;
	  ghookf = [
	    "('text-mode 'flyspell-mode)"
	    "('prog-mode 'flyspell-prog-mode)"
	  ];
	};

	pdf-tools = {
	  generalOneConfig.pdf-view-mode-map."C-s" = "'search-forward";
	  gfhookf = ["('pdf-view-mode 'pdf-view-midnight-minor-mode)"];
	};

	evil-collection-pdf = {
	  defer = true;
	  enable = true;
	  generalTwoConfig.":n".pdf-view-mode-map = {
	    "C-e" = "'pdf-view-scroll-up-or-next-page";
	    "E" = "'pdf-view-scroll-up-or-next-page";
	    "C-o" = "'pdf-view-scroll-down-or-previous-page";
	    "O" = "'pdf-view-scroll-down-or-previous-page";
	  };
	};

        citar.setopt.citar-bibliography = [''"~/doc/uni.bib"''];
      };
    };
  };
}
