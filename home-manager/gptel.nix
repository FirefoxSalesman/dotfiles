{ config, pkgs, inputs, ... }:

{
  programs.emacs.init.usePackage = {
    gptel = {
      enable = true;
      defer = true;
      command = ["start-ollama"];
      custom = {
	gptel-default-mode = "'org-mode";
	gptel-tools = ''
	  
	'';
	gptel-max-tokens = 10000000;
	gptel-prompt-prefix-alist = ''
	  `((markdown-mode . ,(concat "meatbag ›  "))
            (org-mode . ,(concat  "meatbag ›  "))
            (text-mode . ,(concat "meatbag ›  ")))
	'';
	gptel-response-prefix-alist = ''
	  '((markdown-mode . "HK-47  ")
            (org-mode . "HK-47  ")
            (text-mode . "HK-47  "))
	'';
      };
      generalOne.global-leader = {
	"g" = '''(:ignore t :which-key "gptel")'';
	"gs" = '''(start-ollama :which-key "start")'';
	"gp" = '''(gptel :which-key "prompt")'';
      };
      generalTwo."local-leader".gptel-mode-map = {
	"d" = '''(gptel-send :which-key "send")'';
	"m" = '''(gptel-menu :which-key "menu")'';
      };
      preface = ''
	(defun start-ollama ()
	  (interactive)
	  (start-process-shell-command "start-ollama" nil "${(import ./scripts/start-ollama.nix { inherit pkgs config; })}/bin/start-ollama"))
	
	(defun pmx--gptel-symbolp (name)
	  (intern-soft name))
	
	(defun pmx--gptel-manual-names ()
	  (json-serialize (vconcat (info--filter-manual-names
	                            (info--manual-names nil)))))
	
	(defun pmx--gptel-manual-list-nodes (name)
	  (json-serialize
	   (vconcat
	    (mapcar #'car (Info-build-node-completions name)))))
	
	(defun pmx--gptel-manual-node-contents (manual node)
	  (condition-case err
	      (progn
	        (save-window-excursion
	          (Info-goto-node (format "(%s)%s" manual node))
	          (buffer-substring-no-properties (point-min) (point-max))))
	    (user-error
	     (error (error-message-string err)))))
	
	(defun pmx--gptel-symbol-in-manual (symbol)
	  (require 'helpful)
	  (when-let* ((symbol (intern-soft symbol))
	              (_completion (helpful--in-manual-p symbol)))
	    (save-window-excursion
	      (info-lookup 'symbol symbol #'emacs-lisp-mode)
	      (buffer-substring-no-properties (point-min) (point-max)))))
	
	(defun pmx--gptel-featurep (feature)
	  "Return non-nil if FEATURE is loaded or available.
	User might not have FEATURE loaded if it is an autoload etc."
	  (if-let ((feature-symbol (intern-soft feature)))
	      (when (featurep feature-symbol)
	        feature)
	    (find-library-name feature)))
	
	(defun pmx--gptel-features ()
	  (mapconcat #'symbol-name features "\n"))
	
	(defun pmx--gptel-load-paths ()
	  (string-join load-path "\n"))
	
	(defun pmx--gptel-library-source (library-name)
	  "Return the source code of LIBRARY-NAME as a string."
	  (if-let ((library (find-library-name library-name)))
	      (with-temp-buffer
	        (progn
	          (insert-file-contents library)
	          (buffer-string)))
	    (error "Library not found: %s" library-name)))
	
	(defun pmx--gptel-source (symbol &optional type)
	  "Retrieve the source code for SYMBOL of TYPE.
	SYMBOL should be a function or variable name, given as a string or symbol.
	TYPE can be nil for functions, 'defvar for variables, or 'defface for faces.
	Returns the source code as a string, or nil if the definition is not found."
	  (when-let* ((callable (intern-soft symbol))
	              (buffer-point (find-definition-noselect callable type)))
	    (with-current-buffer (car buffer-point)
	      (goto-char (cdr buffer-point))
	      (buffer-substring-no-properties
	       (point)
	       (progn (if (null type)
	                  (end-of-defun)
	                (cond ((derived-mode-p 'c-mode)
	                       (forward-sexp 2)
	                       (forward-char))
	                      ((derived-mode-p 'emacs-lisp-mode)
	                       (forward-sexp))
	                      (t (error "Unexpected file mode"))))
	              (point))))))
	
	(defun pmx--gptel-function-completions (prefix)
	  (require 'orderless)
	  (string-join (orderless-filter prefix obarray #'functionp) "\n"))
	
	(defun pmx--gptel-command-completions (prefix)
	  (require 'orderless)
	  (string-join (orderless-filter prefix obarray #'commandp) "\n"))
	
	(defun pmx--gptel-variable-completions (prefix)
	  (require 'orderless)
	  (string-join (orderless-filter prefix obarray #'boundp) "\n"))
	
	(defun pmx--gptel-function-source (symbol)
	  (when-let ((symbol (intern-soft symbol)))
	    (pmx--gptel-source symbol)))
	
	(defun pmx--gptel-variable-source (symbol)
	  (when-let ((symbol (intern-soft symbol)))
	      (pmx--gptel-source symbol 'defvar)))
	
	(defun pmx--gptel-function-documentation (symbol)
	  (when-let ((symbol (intern-soft symbol)))
	    (documentation symbol)))
	
	(defun pmx--gptel-variable-documentation (symbol)
	  (when-let ((symbol (intern-soft symbol)))
	    (custom-variable-documentation symbol)))
	
	(defun pmx--gptel-variable-global-value (symbol)
	  (when-let ((symbol (intern-soft symbol)))
	    (default-value symbol)))
	
	(defun pmx--gptel-eval (expression)
	  (format "%S" (eval (read expression))))
	
	(defun pmx--gptel-simulate-error ()
	  (error "This is a simulated error message.  OMGWTF."))
	
	(defun pmx--gptel-coerce-nil ()
	  nil)
	
	(defun pmx--gptel-all-arg-types (object string array null true false enum)
	  (message "object: %S\nstring: %S\narray: %S\nnull: %S\ntrue: %S\nfalse: %S\n\
	enum: %S"
	           object string array null true false enum))
	
	(defun pmx--gptel-async-tool (callback later-val)
	  (sit-for 2)
	  (funcall callback (format "Do it %s." later-val)))
      '';
      config = ''
	(start-ollama)
	
	;; (setopt gptel-backend (gptel-make-ollama "Ollama"
	;; 			:stream t
	;; 			:protocol "http"
	;; 			:host "localhost:11434"
	;; 			:models '(qwen3:latest))
	;; 	gptel-prompt-prefix-alist '((default . "You are a large language model and a helpful assistant. Respond concisely.")
	;; 				    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
	;; 				    (writing . "You are a large language model and a writing assistant. Respond concisely.")
	;; 				    (chat . "You are a large language model and a conversation partner. Respond concisely.")))
	
	(setopt gptel-backend (gptel-make-gh-copilot "Copilot")
		gptel-prompt-prefix-alist '((default . "You are a large language model and a helpful assistant. Respond concisely.")
					    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
					    (writing . "You are a large language model and a writing assistant. Respond concisely.")
					    (chat . "You are a large language model and a conversation partner. Respond concisely.")))
      '';
    };

    gptel-quick = {
      enable = true;
      defer = true;
      generalOne = {
	embark-general-map."?" = '''(gptel-quick :which-key "summarize")''; 
	global-leader."gq" = '''(gptel-quick :which-key "summarize")'';
      };
    };

    gptel-aibo = {
      enable = true;
      generalOne.":ie" = {
	"M-/" = "'gptel-aibo-complete-at-point";
	"M-?" = "'gptel-aibo-apply-last-suggestions";
      };
    };

    macher = {
      enable = true;
      command = ["macher-install"];
      generalOne.global-leader = {
	"gi" = "'macher-implement";
	"gr" = "'macher-revise";
	"gd" = "'macher-discuss";
	"ga" = "'macher-abort";
      };
    };
  };
}
