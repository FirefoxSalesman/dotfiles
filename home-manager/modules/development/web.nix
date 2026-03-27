{
  flake.homeModules.development = { config, pkgs, lib, ... }:
  {
    programs.emacs.init.ide.languages = {
      javascript.enable = true;
      html = {
	enable = true;
	emmet = true;
      };
      css = {
	enable = true;
	emmet = true;
      };
    };
  };
}
