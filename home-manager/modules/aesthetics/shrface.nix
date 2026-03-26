{
  flake.homeModules.aesthetics = { ... }:
  {
    programs.emacs.init.usePackage = {
      shr-tag-pre-highlight = {
	enable = true;
	after = ["shrface"];
	preface = ''
	  (defun shrface-shr-tag-pre-highlight (pre)
	        "Highlighting code in PRE."
	        (let* ((shr-folding-mode 'none)
	               (shr-current-font 'default)
	               (code (with-temp-buffer
	                       (shr-generic pre)
	                       (buffer-string)))
	               (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
	                         (let ((sym (language-detection-string code)))
	                           (and sym (symbol-name sym)))))
	               (mode (and lang
	                          (shr-tag-pre-highlight--get-lang-mode lang))))
	          (shr-ensure-newline)
	          (shr-ensure-newline)
	          (setq start (point))
	          (insert
	           (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
	           (or (and (fboundp mode)
	                    (with-demoted-errors "Error while fontifying: %S"
	                      (shr-tag-pre-highlight-fontify code mode)))
	               code)
	           (propertize "\n#+END_SRC" 'face 'org-block-end-line ))
	          (shr-ensure-newline)
	          (setq end (point))
	          (add-face-text-property start end '(:background "#1f2329" :extend t))
	          (shr-ensure-newline)
	          (insert "\n")))
	'';
	config = ''
	  (add-to-list 'shr-external-rendering-functions
	                   '(pre . shrface-shr-tag-pre-highlight))
	'';
      };

      shrface = {
	enable = true;    
	ghookf = ["('(eww-mode elfeed-show-mode nov-mode) 'shrface-mode)"];
	setopt.shrface-header-line-max-level = 0;
	config = ''
	  (defvar shrface-general-rendering-functions
	    (append '((title . eww-tag-title)
	              (form . eww-tag-form)
	              (input . eww-tag-input)
	              (button . eww-form-submit)
	              (textarea . eww-tag-textarea)
	              (select . eww-tag-select)
	              (link . eww-tag-link)
	              (meta . eww-tag-meta)
	              (code . shrface-tag-code))
	            shrface-supported-faces-alist))
	  
	  (defvar shrface-nov-rendering-functions
	    (append '((img . nov-render-img)
	              (svg . nov-render-svg)
	              (title . nov-render-title)
	              (code . shrface-tag-code)
	              (form . eww-tag-form)
	              (input . eww-tag-input)
	              (button . eww-form-submit)
	              (textarea . eww-tag-textarea)
	              (select . eww-tag-select)
	              (link . eww-tag-link)
	              (meta . eww-tag-meta))
	            shrface-supported-faces-alist))
	  
	  (defun shrface-render-advice (orig-fun &rest args)
	    (require 'eww)
	    (let ((shrface-org nil)
	          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
	          (shr-width 91)
	          (shr-max-width 91)
	          (shr-indentation 0)
	          (shr-external-rendering-functions shrface-general-rendering-functions)
	          (shrface-toggle-bullets nil)
	          (shrface-href-versatile t))
	      ;; workaround, need a delay to update the header line
	      (run-with-timer 0.01 nil 'shrface-update-header-line)
	      (apply orig-fun args)))
	  
	  (defun shrface-elfeed-advice (orig-fun &rest args)
	    (require 'eww)
	    (let ((shrface-org nil)
	          ;; make it large enough, it would not fill the column
	          (shr-width 7000)
	          (shr-indentation 0)
	          (shr-external-rendering-functions shrface-general-rendering-functions)
	          (shrface-toggle-bullets nil)
	          (shrface-href-versatile t))
	      (apply orig-fun args)))
	  
	  (defun shrface-nov-render-html ()
	    (require 'eww)
	    (let ((shrface-org nil)
	          (shr-width 7000) ;; make it large enough, it would not fill the column (use visual-line-mode/writeroom-mode instead)
	          (shr-indentation 0) ;; remove all unnecessary indentation
	          (tab-width 8)
	          (shr-external-rendering-functions shrface-nov-rendering-functions)
	          (shrface-href-versatile t)
	          (shr-use-fonts nil)           ; nil to use default font
	          (shr-map nov-mode-map))
	  
	      ;; HACK: `shr-external-rendering-functions' doesn't cover
	      ;; every usage of `shr-tag-img'
	      (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
	        (shr-render-region (point-min) (point-max)))))
	  
	  (with-eval-after-load 'eww (general-advice-add 'eww-display-html :around #'shrface-render-advice))
	  (with-eval-after-load 'elfeed (general-advice-add 'elfeed-insert-html :around #'shrface-elfeed-advice))
	  (with-eval-after-load (gsetq nov-render-html-function 'shrface-nov-render-html))
	  
	  (add-to-list 'evil-fold-list
	               `((shrface-mode)
	                 :open shrface-outline-cycle
	                 :open-all nil
	                 :close shrface-outline-cycle
	                 :close-all nil
	                 :toggle shrface-outline-cycle
	                 :delete nil
	                 :open-rec nil))
	'';
	generalOneConfig.shr-map."RET" = "`,(cmd! (if (major-mode? 'eww-mode) (eww-follow-link) (shr-browse-url)))";
	generalTwoConfig.local-leader.shrface-mode-map = {
	  "l" = "'shrface-links-consult";
	  "o" = "'shrface-headline-consult";
	  "]c" = "'org-next-block";
	  "[c" = "'org-previous-block";
	};
      };
    };
  };
}
