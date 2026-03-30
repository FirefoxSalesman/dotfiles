{
  flake.homeModules.notifications = { ... }:
  {
    programs.emacs.init.usePackage.ednc = {
      enable = true;
      gfhook = [
	"('ednc-notification-presentation-functions #'show-notification-in-echo-area)"
      ];
      deferIncrementally = true;
      config = ''
	(ednc-mode)
	(defun show-notification-in-echo-area (old new)
	  (when new (message (ednc-format-notification new t))))
      '';
    };
  };
}
