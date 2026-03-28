{
  flake.homeModules.emacs = { ... }: {
    programs.emacs.init = {
      completions.smallExtras = {
	enable = true;
	evilConsultLine = true;
      };
      usePackage = {
	consult = {
	  ghookf = ["('minibuffer-setup 'consult-initial-narrow)"];
	  command = ["consult-goto-line" "consult-keep-lines"];
	  setopt = {
	    consult-buffer-sources = "'(consult-source-buffer)";
	    consult-bookmark-narrow = [
	      '''(?b "Bufler" bufler-workspace-bookmark-handler)''
	      '''(?f "File" bookmark-default-handler)''
	      '''(?h "Help" help-bookmark-jump Info-bookmark-jump Man-bookmark-jump woman-bookmark-jump)''
	      '''(?p "Picture" image-bookmark-jump)''
	      '''(?d "Docview" doc-view-bookmark-jump)''
	      '''(?m "Mail" gnus-summary-bookmark-jump)''
	      '''(?s "Eshell" eshell-bookmark-jump)''
	      '''(?w "Web" qutebrowser-bookmark-jump eww-bookmark-jump xwidget-webkit-bookmark-jump-handler)''
	      '''(?v "VC Directory" vc-dir-bookmark-jump)''
	      '''(nil "Other")''
	    ] ;
	  };
	  generalOne = {
	    ":n" = {
	      "M-g" = "'consult-yank-pop"; # orig. evil-paste-pop
	      "M-E" = "'consult-isearch-history "; # orig. isearch-edit-string
	      "H-'" = "'evil-collection-consult-mark";
	      "H--" = "'evil-collection-consult-jump-list";
	      "H-q" = "'consult-flymake"; # Alternative: consult-flycheck
	      "[i" = '''("Previous Imenu" . efs/consult-imenu-previous)'';
	      "]i" = '''("Next Imenu" . efs/consult-imenu-next)'';
	    } ;
	    ctl-x-map."C-f" = "'consult-fd";
	    global-leader."R" = "'consult-recent-file";
	  };
	  bindLocal.help-map."M" = "man";
	  config = ''
	    (defvar consult--bufler-workspace+
	      `(:name "Workspace"
	              :narrow ?w
	              :category buffer
	              :face consult-buffer
	              :history  buffer-name-history
	              :state    ,#'consult--buffer-state
	              :enabled  ,(lambda () (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find)))
	              :items
	              ,(lambda ()
	                 (let ((bufler-vc-state nil))
	                   (mapcar #'buffer-name
	                           (mapcar #'cdr
	                                   (bufler-buffer-alist-at
	                                    (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find))
	                                    :filter-fns bufler-filter-buffer-fns))))))
	      "Bufler workspace buffers source for `consult-buffer'.")
	    
	    (push #'consult--bufler-workspace+ consult-buffer-sources)
	    
	    ;; Stolen from the wiki
	    (defun consult-initial-narrow ()
	      (when (and (eq this-command #'consult-buffer)
	                 (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find)))
	        (setq unread-command-events (append unread-command-events (list ?w 32)))))
	    
	    (defun efs/consult-goto-imenu (filter getter)
	                  "Go to the next imenu item.
	                FILTER is a function used to filter for items. (Such as '>' or '<').
	                GETTER is a function used to get the appropriate item (Such as 'car' or 'last')."
	                  (require 'consult-imenu)
	                  (goto-char (funcall getter
	                		      (-filter (lambda (x)
	                				 (funcall filter x (marker-last-position (point-marker))))
	                			       (mapcar (lambda (x)
	                					 (marker-last-position (cdr x)))
	                				       (consult-imenu--items))))))
	    
	    (defun efs/consult-imenu-next ()
	      "Go to the next imenu item."
	      (interactive)
	      (efs/consult-goto-imenu '> 'car))
	    
	    (defun efs/consult-imenu-previous ()
	      "Go to the previous imenu item."
	      (interactive)
	      (efs/consult-goto-imenu '< (lambda (x) (car (last x)))))
	    
	    (repeaters-define-maps
	     '(("imenu"
	        efs/consult-imenu-next "i"
	        efs/consult-imenu-previous "I")))
	  '';
	};

	bookmark = {
	  enable = true;  
	  generalOne."ctl-x-r-map"."S" = "'bookmark-save";
	};
      };
    };
  };
}
