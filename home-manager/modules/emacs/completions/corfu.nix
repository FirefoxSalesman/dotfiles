{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      completions = {
	tempel.enable = true;
	corfu = {
	  enable = true;
	  wantTabComplete = false;
	  wantRetConfirm = false;
	  wantMinibuffer = true;
	  popupInfo = true;
	};
      };

      usePackage = {
	corfu-quick = {
	  enable = true;
	  setopt = {
	    corfu-quick1 = ''"crst"'';
	    corfu-quick2 = ''"neia"'';
	  };
	};

	cape = {
	  enable = true;
	  after = ["corfu"];
	  config = ''
	    (dolist (src (list 'cape-dabbrev 'cape-file))
	      (add-to-list 'completion-at-point-functions src))
	  '';
	};
      };
    };
  } ;
}
