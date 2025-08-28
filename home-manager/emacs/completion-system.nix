{ inputs, pkgs, ... }:

{
  home.file.".config/emacs/templates.eld".text = ''
      nix-mode
      (upackage p "= {" n "  enable = true;" q "  }")
  
      js-ts-mode
      (clg "console.log(" p ");")
      (doc "/**" n> " * " q n " */")
      (anfn "(" p ") => {" n> q n "};")
      (qs "document.querySelector(\"" q "\");")
      (if "if (" p ") {" n> q n "}")
  
      c-ts-mode
      (doc "/**" n> " * " q n " */")
      (if "if (" p ") {" n> q n "}")
      (for "for (int i = " p "; i < " p "; i++) {" n> q n "}")
      (while "while (" p ") {" n> q n "}")
      (stdio "#include <stdio.h>")
      (stdlib "#include <stdlib.h>")
      (string "#include <string.h>")
      (unistd "#include <unistd.h>")
      (mpi "#include <mpi.h>")
      (math "#include <math.h>")
      (define "#define " p)
      (function p " " p " (" p ") {" n> q n "}" ) 
      (main "int main (int argc, char **argv) {" n> q n "}" ) 
  
      org-mode
      (au "#+author: " q)
      (ti "#+title: " q)
      (ci "* Works Cited" n "#+cite_export: csl ~/.config/csl/ieee.csl" n "#+print_bibliography:" q)
      (pdf "#+auto-export-pandoc: to-latex-pdf")
      (odt "#+auto-export-pandoc: to-odt")
  
      java-ts-mode
      (doc "/**" n> " * " q n " */")
      (if "if (" p ") {" n> q n "}")
      (class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name)))) " {" n> r> n "}")
      (method p " " p " " p "(" p ") {" n> q n "}")
      (while "while (" p ") {" n> q n "}")
      (for "for (int i = " p "; i < " p "; i++) {" n> q n "}")
  
      bash-ts-mode
      (bang "#!/bin/sh" n q)
      (safebang "#!/bin/sh" n "set -euo pipefail" n q)
  
      ledger-mode
      (payroll p "Rose-Hulman Payroll" n> "Income:TeachingAssistant" > "-" p n> "*Assets:Checking")
      (checking "Assets:Checking")
  
      racket-mode
      (let "(let [(" p ")]" n q ")")
      (letrec "(letrec [(" p ")]" n q ")")
      (letstar "(let* [(" p ")]" n q ")")
      (namelet "(let " p " [(" p ")]" n q ")")
      (defun "(define " p " (lambda (" p ")" n q "))")
    '';

  programs.emacs.init.usePackage = {
    vertico = {
      enable = true;
      defer = true;
      ghook = ["('on-first-input-hook 'vertico-mode)"];
      # Enable cycling for 'vertico-next & 'vertico-previous'.
      custom.vertico-cycle = true;
      # This is for exwm's minibuffer issue
      generalTwo."'normal".vertico-map = {
        "RET" = "'vertico-exit";
        "C-o" = "'vertico-scroll-down";
        "C-e" = "'vertico-scroll-up";
        "e" = "'vertico-next";
        "o" = "'vertico-previous";
        "I" = "'vertico-last";
        "N" = "'vertico-first";
        "B" = "'vertico-last";
        "bg" = "'vertico-first";
      };
    };
    
    vertico-quick = {
      enable = true;
      defer = true;
      generalTwo."'normal".vertico-map = {
        "H-o" = "'vertico-quick-jump";
        "H-e" = "'vertico-quick-jump";
      };
    };
    
    corfu = {
      enable = true;
      ghook = [
        "('minibuffer-setup-hook 'corfu-enable-in-minibuffer)"
        "('on-first-buffer-hook 'global-corfu-mode)"
      ];
      custom = {
        corfu-cycle = true;
        corfu-autodelay = "0";
        corfu-auto-prefix = "2";
        corfu-auto = true;
        corfu-on-exact-match = "'show";
      };
      general."M-/" = "'completion-at-point";
      generalOne.corfu-map = {
        "RET" = "nil";
        "TAB" = "nil";
        "S-<return>" = "'corfu-insert";
        "[tab]" = "nil";
      };
      generalTwo."'(insert emacs)".corfu-map = {
        "S-SPC" = "'corfu-insert-separator";
        "C-e" = "'corfu-next";
        "C-p" = "nil";
        "C-o" = "'corfu-previous";
      };
      config = ''
          (defun corfu-enable-in-minibuffer ()
            "Enable Corfu in the minibuffer."
            (when (local-variable-p 'completion-at-point-functions)
              (setq-local corfu-auto t) ;; Enable/disable auto completion
              (corfu-mode 1)))
        '';
    };
    
    nerd-icons-corfu = {
      enable = true;
      config = ''(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)'';
      after = ["corfu"];
    };
    
    corfu-quick = {
      enable = true;
      generalTwo."'(insert emacs)".corfu-map = {
        "M-o" = "'corfu-quick-insert";
        "M-e" = "'corfu-quick-insert";
      };
    };
    
    corfu-popupinfo = {
      enable = true;
      ghook = ["('corfu-mode-hook 'corfu-popupinfo-mode)"];
    };
    
    cape = {
      enable = true;
      after = ["corfu"];
      config = ''
          (dolist (src (list 'cape-dabbrev 'cape-file))
            (add-to-list 'completion-at-point-functions src))
        '';
    };

    prescient = {
      enable = true;
      defer = true;
      config = ''(prescient-persist-mode)'';
      custom.prescient-history-length = "100";
      afterCall = ["on-first-input-hook"];
    };
    
    vertico-prescient = {
      enable = true;
      defer = true;
      ghook = ["('minibuffer-mode-hook 'vertico-prescient-mode)"];
      custom = {
        vertico-prescient-enable-filtering = false;
        vertico-prescient-completion-styles = "'(orderless prescient basic)";
        vertico-prescient-enable-sorting = true;
      };
    };
    
    corfu-prescient = {
      enable = true;
      defer = true;
      ghook = ["('corfu-mode-hook 'corfu-prescient-mode)"];
      custom.corfu-prescient-completion-styles = "'(basic prescient)";
    };

    orderless = {
      enable = true;
      defer = true;
      custom = {
        completion-styles = "'(orderless prescient basic)";
        completion-category-defaults = false;
        completion-category-overrides = "'((file (styles . (partial-completion))))";
      };
      afterCall = ["on-first-input-hook"];
    };

    marginalia = {
      enable = true;
      defer = true;
      config = ''(marginalia-mode)'';
      afterCall = ["on-first-input-hook"];
    };

    consult = {
      enable = true;
      defer = true;
      extraPackages = with pkgs; [
        fd
        ripgrep
      ];
      ghook = ["('minibuffer-setup-hook 'consult-initial-narrow)"];
      command = ["consult-goto-line" "consult-keep-lines" "noct-consult-ripgrep-or-line"];
      general = {
        # https://taonaw.com/2025/05/15/two-eamcs-tweaks-i-forgot.html
        "M-y" = "'nil";
        # C-c bindings (mode-specific-map)
        "C-c m" = "'consult-mode-command";
        "C-c k" = "'consult-kmacro";
        # Custom M-# bindings for fast register access
        "M-#" = "'consult-register-load";
        "M-'" = "'consult-register-store";       # orig. abbrev-prefix-mark (unrelated)
        "C-M-#" = "'consult-register";
        # M-s bindings (search-map)
        "M-s E" = "'consult-compile-error";
        "M-s m" = "'consult-mark";
        "M-s k" = "'consult-global-mark";
        "M-s D" = "'consult-locate";
        "M-s G" = "'consult-git-grep";
        "M-s g" = "'consult-ripgrep";
        "M-s u" = "'consult-focus-lines";
        # Isearch integration
        "M-s e" = "'consult-isearch-history"; # orig. isearch-edit-string
        "M-s L" = "'consult-line-multi"; # needed by consult-line to detect isearch
      };
      custom = {
        consult-buffer-sources = "'(consult--source-buffer)";
        xref-show-xrefs-function = "#'consult-xref";
      };
      generalOne = {
        "'normal" = {
          "?" = "'consult-line-multi";
          "M-g" = "'consult-yank-pop"; # orig. evil-paste-pop
          "M-E" = "'consult-isearch-history "; # orig. isearch-edit-string
          "H-'" = "'evil-collection-consult-mark";
          "H--" = "'evil-collection-consult-jump-list";
          "H-q" = "'consult-flymake"; # Alternative: consult-flycheck
        };  
        global-leader = {
          "b" = '''(consult-bookmark :which-key "bookmarks")'';
          "i" = '''consult-imenu'';
          "I" = '''consult-imenu-multi'';
          "h C-a" = '''(consult-apropos :which-key "apropos")'';
          "h i" = '''(consult-info :which-key "info")'';
          "h M" = '''(consult-man :which-key "man")'';
        };
        ctl-x-map = {
          "M-:" = "'consult-complex-command"; # orig. repeat-complex-command
          "5 b" = "'consult-buffer-other-frame"; # orig. switch-to-buffer-other-frame
          "b" = "'nil";
          "C-f" = "'consult-find";
          "C-r" = "'consult-recent-file";
        };
      };
      config = ''
          (defun efs/save-search-history (pattern)
            "Gets history from pattern, & saves it where evil mode can find it"
            (add-to-history 'evil-search-forward-history pattern)
            (add-to-history 'search-ring pattern)
            (add-to-history 'regexp-search-ring pattern)
            (setq evil-ex-search-pattern (list pattern t t))
            (setq evil-ex-search-direction 'forward)
            (when evil-ex-search-persistent-highlight
              (evil-ex-search-activate-highlight evil-ex-search-pattern)))
          
          (defun noct-consult-line-evil-history (&rest _)
            "Add latest `consult-line' search pattern to the evil search history ring.
            This only works with orderless and for the first component of the search."
            (when (and (bound-and-true-p evil-mode)
                       (eq evil-search-module 'isearch))
              (efs/save-search-history (cadr (orderless-compile
            				    (car consult--line-history))))))
          
          (general-add-advice #'consult-line :after #'noct-consult-line-evil-history)
          
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
          
          (defcustom noct-consult-ripgrep-or-line-limit 1000000
            "Buffer size threshold for `noct-consult-ripgrep-or-line'.
            When the number of characters in a buffer exceeds this threshold,
            `consult-ripgrep' will be used instead of `consult-line'."
            :type 'integer)
          
          (defun noct-consult-ripgrep-or-line (&optional initial)
            "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
            (interactive)
            (if (or (not buffer-file-name)
                    (buffer-narrowed-p)
                    (ignore-errors
                      (file-remote-p buffer-file-name))
                    (jka-compr-get-compression-info buffer-file-name)
                    (<= (buffer-size)
                        (/ noct-consult-ripgrep-or-line-limit
                           (if (eq major-mode 'org-mode) 4 1))))
                (consult-line initial)
              ((lambda ()
                 (when (file-writable-p buffer-file-name)
            	 (save-buffer))
                 (let ((consult-project-function (lambda (x) nil)))
            	 (consult-ripgrep (list (shell-quote-argument buffer-file-name)) (concat " " initial))
            	 (efs/save-search-history (string-trim-left (car consult--grep-history) "# ")))))))
        '';
    } ;

    consult-dir = {
      enable = true;
      defer = true;
      generalOne.vertico-map = {
        "C-x d" = "'consult-dir";
        "C-x j" = "'consult-dir-jump-file";
      };
    };

    embark = {
      enable = true;
      defer = true;
      command = ["embark-act"];
      general."M-a" = "'embark-dwim";
      custom = {
        # Replace key help with a completing-read interface
        prefix-help-command = "#'embark-prefix-help-command";
        embark-indicators = ''
            '(embark-which-key-indicator
              embark-highlight-indicator
              embark-isearch-highlight-indicator)
          '';
      };
      generalTwo."'normal" = {
        embark-collect-mode-map."q" = "'evil-record-macro";
        minibuffer-local-map."a" = "'embark-act";
      };
      generalOne = {
        embark-file-map."2" = "(my/embark-split-action find-file elwm-split-window)";
        embark-buffer-map."2" = "(my/embark-split-action switch-to-buffer elwm-split-window)";
        embark-bookmark-map."2" = "(my/embark-split-action bookmark-jump elwm-split-window)";
      };
      config = ''
          (cl-defun embark--beginning-of-target (&key bounds &allow-other-keys)
            "Go to beginning of the target BOUNDS."
            (goto-char (car bounds)))
          
          (cl-defun embark--end-of-target (&key bounds &allow-other-keys)
            "Go to end of the target BOUNDS."
            (goto-char (cdr bounds)))
        '';
      init = ''
          (defun embark-which-key-indicator ()
            "An embark indicator that displays keymaps using which-key.
                The which-key help message will show the type and value of the
                current target followed by an ellipsis if there are further
                targets."
            (lambda (&optional keymap targets prefix)
              (if (null keymap)
                  (which-key--hide-popup-ignore-command)
                (which-key--show-keymap
                 (if (eq (plist-get (car targets) :type) 'embark-become)
                     "Become"
                   (format "Act on %s '%s'%s"
                           (plist-get (car targets) :type)
                           (embark--truncate-target (plist-get (car targets) :target))
                           (if (cdr targets) "…" "")))
                 (if prefix
                     (pcase (lookup-key keymap prefix 'accept-default)
                       ((and (pred keymapp) km) km)
                       (_ (key-binding prefix 'accept-default)))
                   keymap)
                 nil nil t (lambda (binding)
                             (not (string-suffix-p "-argument" (cdr binding))))))))
          
          (defun embark-hide-which-key-indicator (fn &rest args)
            "Hide the which-key indicator immediately when using the completing-read prompter."
            (which-key--hide-popup-ignore-command)
            (let ((embark-indicators
                   (remq #'embark-which-key-indicator embark-indicators)))
              (apply fn args)))
          
          (advice-add #'embark-completing-read-prompter
                      :around #'embark-hide-which-key-indicator)
          
          (eval-when-compile
            (defmacro my/embark-split-action (fn split-type)
              `(defun ,(intern (concat "my/embark-"
                                       (symbol-name fn)
                                       "-"
                                       (car (last  (split-string
                                                    (symbol-name split-type) "-"))))) ()
                 (interactive)
                 (funcall #',split-type)
                 (call-interactively #',fn))))
          
          ;; Hide the modeline of embark live/completions buffers
          (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none))))
        '';
    } ;
    
    embark-consult = {
      enable = true;
      after = ["embark" "consult"];
      ghook = ["('embark-collect-mode-hook 'consult-preview-at-point-mode)"];
    };

    app-launcher = {
      enable = true;
      defer = true;
      command = ["app-launcher-run-app"];
    };

    ace-isearch = {
      enable = true;
      demand = true;
      gfhook = ["('pdf-view-mode-hook (lambda () (ace-isearch-mode -1)))"];
      generalOne.isearch-mode-map."C-a" = "'avy-isearch";
      config = ''
          (global-ace-isearch-mode)
          (defun ace-isearch-consult-ripgrep-or-line-from-isearch ()
            "Invoke `noct-consult-ripgrep-or-line' from ace-isearch."
            (interactive)
            (let (($query (if isearch-regexp
            		    isearch-string
            		  (regexp-quote isearch-string))))
              (isearch-update-ring isearch-string isearch-regexp)
              (let (search-nonincremental-instead)
                (ignore-errors (isearch-done t t)))
              (noct-consult-ripgrep-or-line $query)))
        '';
      custom = {
        ace-isearch-on-evil-mode = true;
        ace-isearch-input-length = "5";
        ace-isearch-jump-based-on-one-char = false;
        ace-isearch-function-from-isearch = "'ace-isearch-consult-ripgrep-or-line-from-isearch";
      };
    };

    tempel = {
      enable = true;
      command = ["tempel-complete"];
      custom.tempel-path = ''"~/.config/emacs/templates.eld"'';
      init = ''
          (defun tempel-setup-capf ()
            " Add the Tempel Capf to `completion-at-point-functions'.
              `tempel-expand' only triggers on exact matches. Alternatively use
              `tempel-complete' if you want to see all matches, but then you
              should also configure `tempel-trigger-prefix', such that Tempel
              does not trigger too often when you don't expect it. NOTE: We add
              `tempel-expand' *before* the main programming mode Capf, such
              that it will be tried first."
            (setq-local completion-at-point-functions
                        (cons #'tempel-expand
                              completion-at-point-functions)))
        '';
    };
    
    tempel-collection = {
      enable = true;
      after = ["tempel"];
    };

    ezf = {
      enable = true;
      deferIncrementally = true;
    };
  };
}
