{ inputs, pkgs, ... }:

{
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
      tempel.enable = true;
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
        setopt = {
          consult-buffer-sources = "'(consult--source-buffer)";
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

      ezf = {
        enable = true;
        deferIncrementally = true;
      };
    };
  };
}
