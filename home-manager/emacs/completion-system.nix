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
      vertico = {
        enable = true;
        evilConsultLine = true;
        embark = true;
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
      vertico.generalTwo.":n".vertico-map = {
        "C-o" = "'vertico-scroll-down";
        "C-e" = "'vertico-scroll-up";
        "j" = "'evil-undo";
        "I" = "'vertico-last";
        "N" = "'vertico-first";
        "B" = "'vertico-last";
        "bg" = "'vertico-first";
      };
      
      vertico-quick.custom = {
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
        generalTwo.":ie".corfu-map = {
          "M-o" = "'corfu-quick-insert";
          "M-e" = "'corfu-quick-insert";
        };
        custom = {
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
        ghook = ["('minibuffer-setup-hook 'consult-initial-narrow)"];
        command = ["consult-goto-line" "consult-keep-lines"];
        custom.consult-buffer-sources = "'(consult--source-buffer)";
        generalOne = {
          ":n" = {
            "M-g" = "'consult-yank-pop"; # orig. evil-paste-pop
            "M-E" = "'consult-isearch-history "; # orig. isearch-edit-string
            "H-'" = "'evil-collection-consult-mark";
            "H--" = "'evil-collection-consult-jump-list";
            "H-q" = "'consult-flymake"; # Alternative: consult-flycheck
          };
          ctl-x-map."C-f" = "'consult-fd";
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
          '';
      } ;

      embark = {
        general."M-a" = "'embark-dwim";
        generalTwo.":n".vertico-map."a" = "'embark-act";
        generalOne = {
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
        gfhook = ["('pdf-view-mode-hook (lambda () (ace-isearch-mode -1)))"];
        generalOne.isearch-mode-map."C-a" = "'avy-isearch";
        config = "(global-ace-isearch-mode)";
        custom = {
          ace-isearch-on-evil-mode = true;
          ace-isearch-input-length = 5;
          ace-isearch-jump-based-on-one-char = false;
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
  };
}
