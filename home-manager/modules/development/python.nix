{
  flake.homeModules.python = { pkgs, ... }:
  {
    programs.emacs.init = {
      ide.languages.python = {
	enable = true;
	languageServer = "basedpyright";
      };
      usePackage = {
	python-ts-mode.setopt = {
	  python-shell-interpreter = ''"ipython"'';
	  python-shell-interpreter-args = ''"-i --simple-prompt"'';
	};
	eglot-python-preset = {
	  enable = true;
	  extraPackages = with pkgs; [rassumfrassum ruff uv];
	  setopt = {
	    eglot-python-preset-lsp-server = "'rass";
	    eglot-python-preset-rass-tools= ["'ty" "'ruff"];
	  };
	};
      };
    };
  };
}
