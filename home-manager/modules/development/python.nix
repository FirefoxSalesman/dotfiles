{
  flake.homeModules.python = { ... }:
  {
    programs.emacs.init = {
      ide.languages.python = {
	enable = true;
	language-server = "basedpyright";
      };
      usePackage.python-ts-mode.setopt = {
	python-shell-interpreter = ''"ipython"'';
	python-shell-interpreter-args = ''"-i --simple-prompt"'';
      };
    };
  };
}
