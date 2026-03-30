{
  flake.homeModules.emacs = { ... }:
  {
    programs.emacs.init.usePackage.evil-mc = {
      enable = true;
      defer = true;
      command = ["evil-mc-pause-cursors" "evil-mc-make-cursor-here"];
      gfhookf = [''('doom-escape (lambda () (when (and (featurep 'evil-mc) (evil-mc-has-cursors-p))
	(evil-mc-undo-all-cursors)
						 (evil-mc-resume-cursors) t)))''];
      generalOne = {
	":nv"."bz" = "'evil-mc-hydra/body";
	global-leader."C" = "'evil-mc-hydra/body";
      };
      config = ''
	(global-evil-mc-mode)
	
	;; Don't mess with my macros.
	;; https://github.com/gabesoft/evil-mc/issues/83
	(defun ~+multiple-cursors-evil-mc-write-cursor-state-a (state)
	  "Write the state of the real cursor with values from STATE."
	  (let ((names (evil-mc-get-cursor-variables)))
	    (dolist (name names)
	      (when (boundp name)
	        (let ((p (evil-mc-get-cursor-property state name)))
	          (when (not
	                 (or
	                  (eq name 'register-alist)
	                  (eq name 'evil-markers-alist)))
	            (set name p)))))))
	(advice-add #'evil-mc-write-cursor-state :override #'~+multiple-cursors-evil-mc-write-cursor-state-a)
      '';
      extraConfig = ''
	:pretty-hydra
        ((:color pink :pre (evil-mc-pause-cursors))
         ("Search"
          (("m" #'evil-mc-make-and-goto-next-match "Search forward")
           ("M" #'evil-mc-make-and-goto-prev-match "Search backward")
           ("C-m" #'evil-mc-skip-and-goto-next-match "Skip forward")
           ("C-M" #'evil-mc-skip-and-goto-prev-match "Skip backward"))
          "Undo"
          (("q" #'evil-mc-undo-all-cursors)
           ("j" #'evil-mc-undo-last-added-cursor))
          "Pause/Resume"
          (("r" #'evil-mc-resume-cursors "Resume")
           ("p" #'evil-mc-pause-cursors "Pause")
           ("<return>" #'evil-mc-resume-cursors "Quit" :color blue))
          "Create Cursors"
          (("h" #'evil-mc-make-all-cursors "All")
           ("s" #'evil-mc-make-cursor-here "Here")
           ("E" #'evil-mc-make-cursor-move-next-line "Next Line")
           ("O" #'evil-mc-make-cursor-move-prev-line "Prev Line"))))
      '';
    };
  };
}
