{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init.usePackage = {
      evil-org.config = ''
	(evil-define-key 'operator 'evil-org-mode
	  "i" 'evil-forward-char)
	(evil-define-key 'normal 'evil-org-mode
	  "o" 'evil-previous-visual-line
	  "O" 'evil-scroll-up
	  "R" 'evil-org-open-above
	  "S" 'evil-org-open-below
	  "x" 'evil-backward-WORD-begin
	  "X" 'evil-backward-word-begin
	  "d" 'evil-yank)
	(evil-define-key 'symex 'evil-org-mode
	  "R" 'evil-org-open-above
	  "S" 'evil-org-open-below)
	(evil-define-key 'visual 'evil-org-mode
	  "i" 'evil-forward-char
	  "s" evil-inner-text-objects-map)
      '';

      evil-org-agenda = {
	generalTwoConfig.":m".evil-org-agenda-mode-map = {
	  "bn" = "'org-agenda-next-item";
	  "bI" = "'evil-window-bottom";
	  "I" = "'org-agenda-do-date-later";
	  "C-S-i" = "'org-agenda-todo-nextset"; # Original binding "C-S-<right>"
	  "l" = "'org-agenda-diary-entry";
	};
      };
    };
  };
}
