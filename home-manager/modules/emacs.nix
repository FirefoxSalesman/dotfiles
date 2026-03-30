{ inputs, ... }:

{
  flake.homeModules.emacs = { config, pkgs, ... }:

  {
    programs.emacs = {
      enable = true;
      package = (config.lib.nixGL.wrap pkgs.emacs30-gtk3);
      init = {
        enable = true;
        packageQuickstart = false;
        recommendedGcSettings = true;
        usePackageVerbose = false;
        largeFileHandling = true;
	keybinds = {
	  doomEscape.enable = true;
	  undo.enable = true;
	  electricPair.enable = true;
	};
  
	prelude = ''
	  (defalias 'gsetq #'general-setq)
	  (general-unbind "C-h")
	  
	  (defmacro cmd! (&rest body)
	    "Returns (lambda () (interactive) ,@body)
	  A factory for quickly producing interaction commands, particularly for keybinds
	  or aliases. Stolen from Doom."
	    (declare (doc-string 1) (pure t) (side-effect-free t))
	    `(lambda (&rest _) (interactive) ,@body))
	  
	  (defmacro local! (var body)
	    "Creates a lambda that runs setq-local on the variable VAR with the value provided by BODY."
	    `(lambda () (setq-local ,var ,body)))
	  
	  (defun gen-mode-hooks (modes)
	    "Takes a list of symbols, MODES, & appends -mode to them."
	    (mapcar (lambda (mode)
	  	    (intern (concat (symbol-name mode) "-mode")))
	  	  modes))
	  
	  (defmacro efs/evil-collection-remap (fun state map &rest args)
	    "Adds more key definitions directly after running some evil-collection setup function.
	  `FUN` is the evil-collection function to advise.
	  `STATE` is the evil state to bind the keys in.
	  `MAP` is the keymap to bind the keys to.
	  `ARGS` is the actual key definitions."
	    `(general-add-advice ,fun :after
	  		       (lambda () (general-def ,state ,map ,@args))))
	'';

	postlude = ''
	  
	'';
      };
    };
  };
}
