{
  flake.homeModules.emacs = { ... }:
  {
    programs.emacs.init = {
      keybinds.avy = {
        enable = true;
        evilModifierKey = "H";
      };
      usePackage.evil-easymotion = {
	generalOne.":nvo" = {
	  "H-m" = "'evilem-motion-search-next";
	  "H-U" = "'evilem-motion-forward-word-begin";
	  "H-u" = "'evilem-motion-forward-WORD-begin";
	  "H-X" = "'evilem-motion-backward-word-begin";
	  "H-x" = "'evilem-motion-backward-WORD-begin";
	  "H-M" = "'evilem-motion-search-previous";
	  "H-)" = "'evilem-motion-forward-sentence-begin";
	  "H-(" = "'evilem-motion-backward-sentence-begin";
	};
	custom.avy-dispatch-alist = [
	  "'(?l . avy-action-ispell)"
	  "'(?o . nix-emacs-avy-action-embark)"
	  "'(?h . avy-action-helpful)"
	  "'(?g . avy-action-yank)"
	  "'(?p . avy-action-teleport)"
	  "'(?q . nix-emacs-avy-action-fold)"
	];
	setopt = {
	  avy-keys = "'(?c ?r ?s ?t ?b ?f ?n ?e ?i ?a)";
	  avy-all-windows = false;
	};
	config = ''
	  
	'';
      };
    };
  };
}
