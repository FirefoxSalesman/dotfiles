{
  flake.homeModules.development = { ... }:
  {
    programs.emacs.init = {
      ide = {
	hoverDoc = true;
	eglot = {
          enable = true;
          preset = true;
	};
	flymake = {
          enable = true;
          preset = true;
	};
      };
      usePackage = {
	eglot = {
	  preface = "(defvar efs/autoformat t) ";
	  gfhookf = [
	    ''('eglot-managed-mode (local! completion-at-point-functions (list (cape-capf-super #'tempel-complete
	      #'eglot-completion-at-point
											     #'cape-file))))
	    ''
	    "('before-save (lambda () (when (and eglot--managed-mode efs/autoformat) (eglot-format-buffer))))"
	  ];
	  config = ''
	    (efs/evil-collection-remap 'evil-collection-eglot-setup 'normal eglot-mode-map 
	    			   "K" 'evil-substitute)
	  '';
	};

	flymake-popon.setopt.flymake-popon-posframe-extra-arguments = [
	  "':poshandler" "'posframe-poshandler-point-bottom-left-corner-upward"
	  "':parent-frame" false "':refposhandler" "'posframe-refposhandler-xwininfo"
	];

	eglot-java = {
	  setopt.eglot-java-user-init-opts-fn = "'eglot-java-init-opts";
	  preface = ''
	    (defun eglot-java-init-opts (server eglot-java-eclipse-jdt)
                   '(:bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]))
	  '';
	};
      };
    };
  };
}
