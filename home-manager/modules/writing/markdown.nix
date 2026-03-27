{
  flake.homeModules.writing = { ... }: {
    programs.emacs.init = {
      ide.languages.markdown.enable = true;
      usePackage.markdown = {
	generalOneConfig.markdown-mode-map."C-c C-e" = "'markdown-do";
	gfhookf = ["('markdown-mode 'efs/markdown-font-setup)"];
	setopt = {
	  markdown-command = ''"multimarkdown"'';
	  markdown-hide-markup = true;
	};
	generalTwoConfig = {
	  ":nm".gfm-mode-map = {
	    "[h" = "'markdown-previous-visible-heading";
	    "]h" = "'markdown-next-visible-heading";
	  };  
	};
	preface = ''
	  (defun efs/markdown-font-setup ()
	    (variable-pitch-mode)
	    (dolist (face '((markdown-header-face-1 . 1.4)
	                    (markdown-header-face-2 . 1.2)
	                    (markdown-header-face-3 . 1.1)
	                    (markdown-header-face-4 . 1.05)
	                    (markdown-header-face-5 . 1.05)
	                    (markdown-header-face-6 . 1.05)))
	      (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face))))
	'';
      };
    };
  };
}
