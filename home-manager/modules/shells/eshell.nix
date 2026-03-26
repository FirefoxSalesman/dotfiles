{
  flake.homeModules.shellConfig = { pkgs, ... }:
  {
    programs.emacs.init = {
      completions.tempel.templates.eshell-mode.gbc = ''"(get-buffer-create \"" q "\")"'';
      terminals = {
	eshell = true;
	eat = true;
      };
      usePackage = {
	eshell = {
	  ghookf = ["('eshell-first-time-mode 'efs/configure-eshell)"];
	  general."s-<enter>" = "'efs/make-eshell";
	  init = ''
	    (defun efs/make-eshell ()
	      (interactive)
	      (eshell 'N))
	  '';
	  config = ''
	    (defun efs/configure-eshell ()
	      ;; Bind some useful keys for evil-mode
	      (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
	      (evil-normalize-keymaps)
	      (gsetq eshell-command-aliases-list '(("gc" "torsocks git clone")
	      				       ("nixbuild" "home-manager switch --flake ~/.config/home-manager/#holschcc")
	      				       ("l" "ls $*")
	      				       ("halt" "doas shutdown -P now")
	    				       ("reboot" "doas reboot")
	      				       ("systembuild" "doas nix run 'github:numtide/system-manager' -- switch --flake '/etc/system-manager/'"))))
	    
	    ;; https://xenodium.com/rinku-cli-link-previews
	    (defun adviced:eshell/cat (orig-fun &rest args)
	      "Like `eshell/cat' but with image support."
	      (if (seq-every-p (lambda (arg)
	                         (and (stringp arg)
	                              (file-exists-p arg)
	                              (image-supported-file-p arg)))
	                       args)
	          (with-temp-buffer
	            (insert "\n")
	            (dolist (path args)
	              (let ((newpath (expand-file-name path)))
	    	    (insert-image (create-image
	    			   newpath (image-type-from-file-name newpath)
	    			   nil :max-width 350)))
	              (insert "\n"))
	            (insert "\n")
	            (buffer-string))
	        (apply orig-fun args)))
	    
	    (advice-add #'eshell/cat :around #'adviced:eshell/cat)
	  '';
	};

	fish-completion.gfhookf = ["('fish-completion-mode (local! completion-at-point-functions '(tempel-complete pcomplete-completions-at-point)))"];

	evil-collection-eshell = {
	  enable = true;
	  defer = true;
	  generalTwoConfig.":n".eshell-mode-map = {
	    "v" = "'evil-collection-eshell-evil-delete";
	    "V" = "'evil-collection-eshell-evil-change";
	    "C-v" = "'evil-collection-eshell-evil-delete-line";
	  };
	  config = ''
	    (efs/evil-collection-remap 'evil-collection-eshell-setup-keys 'normal eshell-mode-map
	    			   "d" 'evil-yank
	    			   "D" 'evil-yank-line
	    			   "c" 'evil-visual-state
	    			   "C" 'evil-visual-line)
	  '';
	};
      };
    };
  };
}
