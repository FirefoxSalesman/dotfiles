{
  flake.homeModules.exwm = { lib, ... }:
  {
    programs.emacs.init.usePackage = {
      javelin = {
	enable = true;
	gfhookf = ["('kill-emacs 'javelin-clear-all)"];
	custom.javelin-disable-confirmation = true;
	general = {
	  "s-1" = "'javelin-go-to-1";
	  "s-2" = "'javelin-go-to-2";
	  "s-3" = "'javelin-go-to-3";
	  "s-4" = "'javelin-go-to-4";
	  "s-5" = "'javelin-go-to-5";
	  "s-6" = "'javelin-go-to-6";
	  "s-7" = "'javelin-go-to-7";
	  "s-8" = "'javelin-go-to-8";
	  "s-9" = "'javelin-go-to-9";
	  "s--" = "'javelin-toggle-quick-menu";
	  "s-h" = "'javelin-add-file";
	  "s-H" = "'javelin-clear";
	};
      };
      exwm.custom.exwm-input-prefix-keys = lib.mkForce [
	'''?\s-1''
	'''?\s-2''
	'''?\s-3''
	'''?\s-4''
	'''?\s-5''
	'''?\s-6''
	'''?\s-7''
	'''?\s-8''
	'''?\s-9''
	'''?\s--''
	'''?\s-h''
	'''?\s-H''
      ];
    };
  };
}
