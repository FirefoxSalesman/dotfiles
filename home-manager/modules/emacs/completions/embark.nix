{ inputs, ... }:

{
  perSystem = { pkgs, ... }:
  let epkgs = pkgs.emacs.pkgs;
  in {
    packages.embark = epkgs.callPackage (
      {
        org,
        consult,
        avy,
        compat,
        elpaBuild,
        fetchurl,
        lib,
      }:
      elpaBuild {
        pname = "embark";
        ename = "embark";
        version = "1.1";
        src = fetchurl {
	  url = "https://elpa.gnu.org/packages/embark-1.1.tar";
	  sha256 = "074ggh7dkr5jdkwcndl6znhkq48jmc62rp7mc6vjidr6yxf8d1rn";
        };
        packageRequires = [
	  org
	  consult
	  avy
	  compat
        ];
        meta = {
	  homepage = "https://elpa.gnu.org/packages/embark.html";
	  license = lib.licenses.free;
        };
      }
    ) { };
  };

  flake.homeModules.emacs = { pkgs, ... }: {
    programs.emacs.init = {
      completions.smallExtras.embark = true;
      usePackage.embark = {
	general."M-a" = "'embark-dwim";
	generalTwo.":n".vertico-map."a" = "'embark-act";
	generalOneConfig = {
	  embark-file-map = {
	    "2" = "(my/embark-split-action find-file elwm-split-window)";
	    "t" = "(my/embark-split-action find-file tab-new)";
	    "o" = "(my/embark-ace-action find-file)";
	  };
	  embark-buffer-map = {
	    "2" = "(my/embark-split-action switch-to-buffer elwm-split-window)";
	    "t" = "(my/embark-split-action switch-to-buffer tab-new)";
	    "o" = "(my/embark-ace-action switch-to-buffer)";
	  };
	  embark-bookmark-map = {
	    "2" = "(my/embark-split-action bookmark-jump elwm-split-window)";
	    "t" = "(my/embark-split-action bookmark-jump tab-new)";
	    "o" = "(my/embark-ace-action bookmark-jump)";
	  };
	};
	preface = ''
	  (eval-when-compile
	    (defmacro my/embark-split-action (fn split-type)
	      `(defun ,(intern (concat "my/embark-"
	                               (symbol-name fn)
	                               "-"
	                               (car (last  (split-string
	                                            (symbol-name split-type) "-"))))) ()
	         (interactive)
	         (funcall #',split-type)
	         (call-interactively #',fn))))
	  
	  (eval-when-compile
	    (defmacro my/embark-ace-action (fn)
	      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
	         (interactive)
	         (with-demoted-errors "%s"
	  	 (require 'ace-window)
	  	 (let ((aw-dispatch-always t))
	             (aw-switch-to-window (aw-select nil))
	             (call-interactively (symbol-function ',fn)))))))
	'';
      };
    };
  };
}
