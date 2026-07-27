{
  flake.homeModules.easysession = { ... }: {
    programs.emacs.init.usePackage.easysession = {
      enable = true;
      config = "(easysession-setup)";
      setopt = {
	easysession-save-interval = 600;
	easysession-switch-to-save-session = true;
	easysession-switch-to-exclude-current = false;
	easyession-setup-load-session = true;
      };
    };
  };
}
