{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage.async = {
      enable = true;
      config = ''
	(autoload 'dired-async-mode "dired-async.el" nil t)
	(dired-async-mode)
      '';
    };
  };
}
