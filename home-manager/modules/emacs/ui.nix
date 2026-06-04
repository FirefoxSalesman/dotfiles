{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      aesthetics.declutter.enable = true;
      tools = {
	noLittering.enable = true;
	lineNumbers.enable = true;
	sensibleDefaults.enable = true;
      };
      usePackage = {
	display-line-numbers.setopt.display-line-numbers-type = "'relative";

	server = {
	  enable = true;
	  deferIncrementally = true;
	  config = "(server-start)";
	};
      };
    };
  };
}
