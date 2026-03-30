{ inputs, ... }:

{
  perSystem = { pkgs, self', ... }: {
    packages.wiki = pkgs.writeShellScriptBin "wiki" ''
      dir="/usr/share/doc/arch-wiki/html/en/"
      doc="$(${pkgs.findutils}/bin/find $dir -iname "*.html" | \
      ${pkgs.coreutils}/bin/cut -d '/' -f8- | \
      ${self'.packages.ezf}/bin/ezf)"
      
      if [ "''${dir}$doc" ]; then
        emacsclient -e "(eww-open-file \"''${dir}$doc\")"
      else
        ${pkgs.coreutils}/bin/echo "Program terminated." && exit 0
      fi
    '';
  };

  flake.homeModules.emacs = { lib, pkgs, ... }: {
    programs.emacs.init = {
      keybinds.whichKey.enable = true;
      usePackage = {
	helpful = {
	  enable = true;
	  defer = true;
	  generalOne = {
            global-leader = {
              "hf" = "'helpful-function";
              "hs" = "'helpful-symbol";
              "hv" = "'helpful-variable";
              "hx" = "'helpful-command";
              "hk" = "'helpful-key";
              "hm" = "'helpful-macro";
            };
            embark-become-help-map = {
              "f" = "'helpful-function";
              "s" = "'helpful-symbol";
              "v" = "'helpful-variable";
            };
            embark-symbol-map."h" = "'helpful-symbol";
	  };
	};

	info = {
	  enable = true;
	  config = ''
	    (with-eval-after-load 'evil-collection-info
	      (efs/evil-collection-remap 'evil-collection-info-setup '(normal motion) Info-mode-map
	    			     "C-o" 'evil-scroll-page-up))
	  '';
	};

	evil-owl = {
	  enable = true;
	  setopt = {
	    evil-owl-max-string-length = 50;
	    evil-owl-extra-posframe-args = [ "':width" 50 "':height" 20 ];
	    evil-owl-display-method = "'posframe";
	  };
	  ghookf = ["('evil-mode 'evil-owl-mode)"];
	};

	emacs.generalOne.help-map."A" = ''`("Arch Wiki" . ,(cmd! (async-shell-command "${lib.getExe pkgs.wiki}")))'';
      } ;
    };
  };
}
