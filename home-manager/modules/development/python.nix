{
  flake.homeModules.python =
    { pkgs, ... }:
    {
      programs.emacs.init = {
        ide.languages.python = {
          enable = true;
          wantRuff = true;
        };
        usePackage.python-ts-mode.setopt = {
          python-shell-interpreter = ''"ipython"'';
          python-shell-interpreter-args = ''"-i --simple-prompt"'';
        };
      };
    };
}
