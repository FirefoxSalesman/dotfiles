{
  flake.homeModules.emacs = { pkgs, ... }: {

    programs.emacs.init.usePackage = {
      ace-isearch = {
	enable = true;
	demand = true;
	gfhookf = ["('pdf-view-mode (lambda () (ace-isearch-mode -1)))"];
	generalOneConfig.isearch-mode-map."C-a" = "'avy-isearch";
	config = "(global-ace-isearch-mode)";
	setopt = {
	  ace-isearch-on-evil-mode = true;
	  ace-isearch-input-length = 5;
	  ace-isearch-jump-based-on-one-char = false;
	  isearch-wrap-pause = "'no-ding";
	  isearch-lazy-count = true;
	};
      };

      replace = {
	enable = true;  
	defer = true;
	generalTwoConfig.":n".occur-mode-map."w" = "'occur-edit-mode";
      };
    };
  };
}
