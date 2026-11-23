{
  flake.homeModules.emacs.programs.emacs.init = {
    aesthetics.declutter.enable = true;
    tools = {
      noLittering.enable = true;
      lineNumbers.enable = true;
      sensibleDefaults.enable = true;
    };
    usePackage = {
      display-line-numbers.setopt.display-line-numbers-type = "'relative";

      files = {
        enable = true;
        setopt = {
          confirm-kill-processes = false;
          trusted-content = [
            ''"~/projects/generic-todo/"''
            ''"~/projects/DungeonsLibs/"''
          ];
          safe-local-variable-directories = [
            ''"~/projects/generic-todo/"''
            ''"~/projects/DungeonsLibs/"''
          ];
        };
      };

      server = {
        enable = true;
        deferIncrementally = true;
        config = "(server-start)";
      };

      loaddefs = {
        enable = true;
        hook = [
          {
            names = [ "on-first-file" ];
            actions = [ "'save-place-mode" ];
          }
        ];
      };
    };
  };
}
