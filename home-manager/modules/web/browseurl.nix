{
  flake.homeModules.web = { ... }:
  {
    programs.emacs.init.usePackage.browse-url = {
      enable = true;
      config = "(defun qutebrowser-browse-url (url &rest args) (qutebrowser-open-url url 'tab))";
      custom.browse-url-handlers = [
	'''("https:\\/\\/www\\.youtu\\.*be." . efs/mpv-browse-url)''
	'''("https:\\/\\/yewtu\\.*be." . efs/mpv-browse-url)''
	'''("https:\\/\\/inv\\.*nadeko\\.*net\\/watch." . efs/mpv-browse-url)''
	'''("search\\.nixos\\.org[^z-a]*" . qutebrowser-browse-url)''
	'''("melpa\.org\.*" . qutebrowser-browse-url)''
	'''("." . (lambda (url &rest args) (eww url (prefix-numeric-value 4))))''
      ];
      setopt.browse-url-secondary-browser-function = "'browse-url-default-browser";
    };
  };
}
