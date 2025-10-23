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
    (exp "#+export: " q)
  
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
  
    emacs-lisp-mode
    (wcd "(with-current-buffer " "q)")
    (gbc "(get-buffer-create " "q)")
  
    eshell-mode
    (gbc "(get-buffer-create \"" q "\")")
  '';

  programs.emacs.init = {
    completions = {
      prescient = true;
      orderless = true;
      vertico.enable = true;
      smallExtras = {
	enable = true;
	embark = true;
	evilConsultLine = true;
      };
      corfu = {
        enable = true;
        wantTabComplete = false;
        wantRetConfirm = false;
        wantMinibuffer = true;
        popupInfo = true;
      };
    };

    usePackage = {
      vertico = {
        generalTwoConfig.":n".vertico-map = {
          "C-o" = "'vertico-scroll-down";
          "C-e" = "'vertico-scroll-up";
          "j" = "'evil-undo";
          "I" = "'vertico-last";
          "N" = "'vertico-first";
          "B" = "'vertico-last";
          "bg" = "'vertico-first";
          "G" = "'evil-paste-after";
        };
        config = ''
          (with-eval-after-load 'evil-collection-vertico
            (efs/evil-collection-remap 'evil-collection-vertico-setup 'normal vertico-map
          			     "k" 'evil-delete-char))
        '';
      };
      
      vertico-quick.setopt = {
        vertico-quick1 = ''"crst"'';
        vertico-quick2 = ''"neia"'';
      };
      
      nerd-icons-corfu = {
        enable = true;
        config = ''(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)'';
        after = ["corfu"];
      };
      
      corfu-quick = {
        enable = true;
        setopt = {
          corfu-quick1 = ''"crst"'';
          corfu-quick2 = ''"neia"'';
        };
      };
      
      cape = {
        enable = true;
        after = ["corfu"];
        config = ''
          (dolist (src (list 'cape-dabbrev 'cape-file))
            (add-to-list 'completion-at-point-functions src))
        '';
      };

      consult = {
        ghookf = ["('minibuffer-setup 'consult-initial-narrow)"];
        command = ["consult-goto-line" "consult-keep-lines"];
        setopt.consult-buffer-sources = "'(consult--source-buffer)";
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
          global-leader."i" = "'efs/consult-header";
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
            
            (defun efs/consult-header ()
              "Runs 'consult-imenu', unless 'consult-outline' or 'consult-org-heading' can be run."
              (interactive)
              (cond ((eq major-mode 'org-mode) (consult-org-heading))
            	((eq major-mode 'gfm-mode) (consult-outline))
            	(t (consult-imenu))))
            
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

      embark = {
        general."M-a" = "'embark-dwim";
        generalTwo.":n".vertico-map."a" = "'embark-act";
        generalOneConfig = {
          embark-file-map = {
            "2" = "(my/embark-split-action find-file elwm-split-window)";
            "t" = "(my/embark-split-action find-file tab-new)";
            "o" = "(my/embark-ace-action find-file)";
          };
          embark-buffer-map = {
            "2" = "(my/embark-split-action switch-to-buffer elwm-split-window)";
            "t" = "(my/embark-split-action switch-to-buffer tab-new)";
            "o" = "(my/embark-ace-action switch-to-buffer)";
          };
          embark-bookmark-map = {
            "2" = "(my/embark-split-action bookmark-jump elwm-split-window)";
            "t" = "(my/embark-split-action bookmark-jump tab-new)";
            "o" = "(my/embark-ace-action bookmark-jump)";
          };
        };
        preface = ''
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
          
          (eval-when-compile
            (defmacro my/embark-ace-action (fn)
              `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
                 (interactive)
                 (with-demoted-errors "%s"
          	 (require 'ace-window)
          	 (let ((aw-dispatch-always t))
                     (aw-switch-to-window (aw-select nil))
                     (call-interactively (symbol-function ',fn)))))))
        '';
      };

      app-launcher = {
        enable = true;
        defer = true;
        command = ["app-launcher-run-app"];
      };

      ace-isearch = {
        enable = true;
        demand = true;
        gfhookf = ["('pdf-view-mode (lambda () (ace-isearch-mode -1)))"];
        generalOneConfig.isearch-mode-map."C-a" = "'avy-isearch";
        config = "(global-ace-isearch-mode)";
        setopt = {
          ace-isearch-on-evil-mode = true;
          ace-isearch-input-length = 5;
          ace-isearch-jump-based-on-one-char = false;
          isearch-wrap-pause = "'no-ding";
        };
      };
      
      replace = {
        enable = true;  
        defer = true;
        generalTwoConfig.":n".occur-mode-map."w" = "'occur-edit-mode";
      };

      tempel = {
        enable = true;
        command = ["tempel-complete"];
        setopt.tempel-path = ''"~/.config/emacs/templates.eld"'';
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
  };
}
