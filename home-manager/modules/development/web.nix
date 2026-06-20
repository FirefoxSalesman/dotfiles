{
  flake.homeModules.webdev = { config, pkgs, lib, ... }:
  {
    programs.emacs.init.ide.languages = {
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
  };
}
