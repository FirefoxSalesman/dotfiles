{
  flake.homeModules.development = { ... }:
  {
    programs.emacs.init = {
      usePackage.python-ts-mode.setopt = {
	python-shell-interpreter = ''"ipython"'';
	python-shell-interpreter-args = ''"-i --simple-prompt"'';
      };
    };
  };
}
