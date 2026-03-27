{
flake.homeModules.java = { ... }:
  {
    programs.emacs.init = {
      ide.languages = {
	java = {
	  enable = true;
	  moreEglot = true;
	};
        gradle.enable = true;
      };
      usePackage.java-ts-mode = {
	preface = ''
	  (defun tkj/java-decompile-class ()
	    "Run the FernFlower decompiler on the current .class file using
	   fernflower, and opens the decompiled Java file."
	    (interactive)
	    (let* ((current-file (buffer-file-name))
	           (output-dir (concat (file-name-directory current-file) "decompiled/"))
	           (decompiled-file (concat output-dir (file-name-base current-file) ".java"))
	           (command (format "fernflower %s %s"
	                            (shell-quote-argument current-file)
	                            (shell-quote-argument output-dir))))
	      (if (and current-file (string-equal (file-name-extension current-file) "class"))
	          (progn
	            (unless (file-directory-p output-dir)
	              (make-directory output-dir t))
	            (message "Running FernFlower decompiler...")
	            (shell-command command)
	            (if (file-exists-p decompiled-file)
	                (find-file decompiled-file)
	              (message "Error: Decompiled file not found at %s" decompiled-file)))
	        (message "Error: This command can only be run on .class files"))))
	'';
	generalTwoConfig.":n".java-ts-mode-map = {
	  "S" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-below "block_comment"))'';
	  "R" = ''`,(cmd! (nix-emacs/starred-evil-open 'evil-open-above "block_comment"))'';
	  "o" = "'evil-previous-visual-line";
	  "O" = "'evil-scroll-up";
	};
      };
    };
  };
}
