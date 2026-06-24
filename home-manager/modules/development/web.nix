{
  flake.homeModules.webdev = { config, pkgs, lib, ... }:
  {
    programs.emacs.init = {
      ide.languages = {
	javascript = {
	  enable = true;
	  wantOxlint = true;
	  wantOxfmt = true;
	};
	html = {
	  enable = true;
	  emmet = true;
	};
	css = {
	  enable = true;
	  emmet = true;
	};
      };

      usePackage = {
	jtsx = {
	  enable = true;
	  mode = [
	    ''("\\.jsx?\\'" . jtsx-jsx-mode)''
	    ''("\\.tsx\\'" . jtsx-tsx-mode)''
	  ];
	  ghookf = ["('(jtsx-jsx-mode jtsx-tsx-mode) '(hs-minor-mode emmet-mode))"];
	  config = ''
	    (dolist (lang '(javascript jsdoc tsx typescript))
	      (unless (treesit-language-available-p lang)
	        (jtsx-install-treesit-language lang)))
	  '';
	  symex = true;
	  eglot = true;
	  generalTwoConfig = {
	    local-leader."'(jtsx-jsx-mode-map jtsx-tsx-mode-map)" = {
	      "z" = "'jtsx-jump-jsx-element-tag-dwim";
	      "R" = "'jtsx-rename-jsx-element";
	      "w" = "'jtsx-wrap-in-jsx-element";
	      "u" = "'jtsx-unwrap-jsx";
	      "c" = "'jtsx-jump-jsx-opening-tag";
	      "o" = "'jtsx-jump-jsx-closing-tag";
	      "t" = "'jtsx-toggle-jsx-attributes-orientation";
	      "h" = "'jtsx-rearrange-jsx-attributes-horizontally";
	      "d" = "'jtsx-rearrange-jsx-attributes-vertically";
	    };
	    ":n"."'(jtsx-jsx-mode-map jtsx-tsx-mode-map)" = {
	      "M-i" = "'jtsx-move-jsx-element-tag-forward";
	      "M-n" = "'jtsx-move-jsx-element-tag-backward";
	      "C-M-i" = "'jtsx-move-jsx-element-forward";
	      "C-M-n" = "'jtsx-move-jsx-element-backward";
	      "C-M-I" = "'jtsx-move-jsx-element-step-in-forward";
	      "C-M-N" = "'jtsx-move-jsx-element-step-in-backward";
	    };
	  };
	};
	emmet.config = ''
	  (dolist (mode '(jtsx-jsx-mode jtsx-tsx-mode))
	    (add-to-list 'emmet-jsx-major-modes mode))
	'';
      };
    };
  };
}
