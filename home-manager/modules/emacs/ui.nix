{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage = {
      tooltip = {
	enable = true;
	config = ''
	  (tooltip-mode -1)
          (set-fringe-mode -1)
	'';
      };

      simple = {
	enable = true;
	config = ''
	  (gsetq save-interprogram-paste-before-kill t)
          (column-number-mode)
	'';
      };

      display-line-numbers = {
	enable = true;
	setopt = {
	  display-line-numbers-type = "'relative";
	  display-line-numbers-width = 3;
	}; 
	config = "(global-display-line-numbers-mode)";
	#Disable line numbers for some modes
	ghookf = ["((gen-mode-hooks '(org term dired eww eat markdown help helpful Info Man shell pdf-view elfeed-search elfeed-show eshell racket-repl sage-shell nov)) (lambda () (display-line-numbers-mode 0)))"];
      } ;

      server = {
	enable = true;
	deferIncrementally = true;
	config = "(server-start)";
      };
    };
  } ;
}
