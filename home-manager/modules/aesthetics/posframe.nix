{
  flake.homeModules.aesthetics = { ... }:
  {
    programs.emacs.init = {
      keybinds.whichKey.posframe.enable = true;
      usePackage = {
	which-key-posframe.config = ''
          (defun which-key-posframe--show-buffer (act-popup-dim)
             "Show which-key buffer when popup type is posframe.
           Argument ACT-POPUP-DIM has the form (HEIGHT . WIDTH), specifying
           the dimensions of the buffer text to be displayed in the popup."
             (when (posframe-workable-p)
               (save-window-excursion
                 (posframe-show
                  which-key--buffer
                  :font which-key-posframe-font
                  :position (point)
                  :poshandler which-key-posframe-poshandler
                  :background-color (face-attribute 'which-key-posframe :background nil t)
                  :foreground-color (face-attribute 'which-key-posframe :foreground nil t)
                  :height (car act-popup-dim)
                  :width (cdr act-popup-dim)
                  :lines-truncate t
                  :internal-border-width which-key-posframe-border-width
                  :internal-border-color (face-attribute 'which-key-posframe-border
                                                         :background nil t)
                  :refposhandler 'vertico-posframe-refposhandler-default
                  :override-parameters which-key-posframe-parameters))))
	'';
	vertico-posframe = {
	  enable = true;
	  defer = true;
	  ghookf = ["('vertico-mode 'vertico-posframe-mode)"];
	  config = ''(set-face-attribute 'vertico-posframe-face nil :family 'variable-pitch)'';
	};
      };
    };
  };
}
