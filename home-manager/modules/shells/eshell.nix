{
  flake.homeModules.shellConfig =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      programs.emacs.init = {
        completions.tempel.templates.eshell-mode.gbc = ''"(get-buffer-create \"" q "\")"'';
        terminals = {
          eshell = true;
          eat = true;
        };
        usePackage = {
          eshell = {
            ghookf = [ "('eshell-first-time-mode 'efs/configure-eshell)" ];
            gfhookf = [
              ''
                ('eshell-post-command (lambda () (eshell-write-history eshell-history-file-name t)
                		                (eshell-read-history eshell-history-file-name t)))
              ''
            ];
            general."s-<enter>" = "'efs/make-eshell";
            setopt = {
              eshell-history-append = true;
              eshell-save-history-on-exit = false;
            };
            init = ''
              	    (defun efs/make-eshell ()
              	      (interactive)
              	      (eshell 'N))
              	  '';
            config = ''
              (defun efs/configure-eshell ()
                ;; Bind some useful keys for evil-mode
                (evil-define-key
                 '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
                (evil-normalize-keymaps))
              
              ;; https://xenodium.com/rinku-cli-link-previews
              (defun adviced:eshell/cat (orig-fun &rest args)
                "Like `eshell/cat' but with image support."
                (if (seq-every-p
                     (lambda (arg)
                       (and (stringp arg)
                            (file-exists-p arg)
                            (image-supported-file-p arg)))
                     args)
                    (with-temp-buffer
                      (insert "\n")
                      (dolist (path args)
                        (let ((newpath (expand-file-name path)))
                          (insert-image
                           (create-image newpath
                                         (image-type-from-file-name newpath)
                                         nil
                                         :max-width 350)))
                        (insert "\n"))
                      (insert "\n")
                      (buffer-string))
                  (apply orig-fun args)))
              
              (advice-add #'eshell/cat :around #'adviced:eshell/cat)
              (advice-add 'eshell-read-aliases-list :after (lambda (&rest _)
                              (dolist (alias '(${
                                lib.optionals (config.home.shellAliases != { }) (
                                  lib.concatStringsSep " " (
                                    lib.mapAttrsToList (n: v: ''("${n}" "${v} $*")'') config.home.shellAliases
                                  )
                                )
                              }))
                                            (add-to-list 'eshell-command-aliases-list alias))))
            '';
          };

          fish-completion.gfhookf = [
            "('fish-completion-mode (local! completion-at-point-functions (list 'tempel-complete 'pcomplete-completions-at-point)))"
          ];

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

          esh-help = {
            enable = true;
            after = [ "eshell" ];
            gfhookf = [ "('eshell-mode 'eldoc-box-hover-at-point-mode)" ];
            config = "(setup-esh-help-eldoc)";
          };

          popper.setopt.popper-reference-buffers = [
            ''"^\\*.*eshell\\*"''
            ''"^\\*eat\\*"''
          ];
        };
      };
    };
}
