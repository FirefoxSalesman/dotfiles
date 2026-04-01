{
  flake.homeModules.web = { ... }: {
    programs.emacs.init.usePackage.eww = {
      enable = true;
      setopt = {
	eww-search-prefix = ''"https://search.inetol.net/?q="'';
	eww-header-line-format = false;
	eww-desktop-remove-duplicates = true;
	eww-download-directory = ''(expand-file-name "~/dwn")'';
	eww-history-limit = 10;
	# External Browser
	eww-use-external-browser-for-content-type = ''"\\`\\(video/\\|audio\\)"''; # On GNU/Linux check your mimeapps.list
	eww-browse-url-new-windowis-tab = false;
	eww-form-checkbox-selected-symbol = ''"[X]"'';
	eww-form-checkbox-symbol = ''"[ ]"'';
	eww-auto-rename-buffer = "'title";
      };
      generalTwoConfig.":n".eww-mode-map = {
	"N" = "#'eww-back-url";
	"I" = "#'eww-forward-url";
	"P" = "#'eww-copy-page-url";
	"R" = "'eww-readable";
      };
      init = ''
	(with-eval-after-load 'evil-collection-eww
	  (efs/evil-collection-remap 'evil-collection-eww-setup 'normal eww-mode-map
				     "d" 'evil-yank
				     "p" 'efs/mpv-eww-url))
      '';
    };
  };
}
