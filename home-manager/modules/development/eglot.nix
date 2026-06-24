{
  flake.homeModules.development =
    { pkgs, ... }:
    {
      programs.emacs.init = {
        ide = {
          hoverDoc = true;
          eglot = {
            enable = true;
            preset = true;
          };
          flymake = {
            enable = true;
            preset = true;
          };
        };

        # https://github.com/radian-software/apheleia/issues/153
        tools.apheleia.formatters.eglot = ''
          (cl-defun apheleia-indent-eglot-managed-buffer
                  (&key buffer scratch callback &allow-other-keys)
                "Copy BUFFER to SCRATCH, then format scratch, then call CALLBACK."
                (with-current-buffer scratch
                  (setq-local eglot--cached-server
                              (with-current-buffer buffer
                                (eglot-current-server)))
                  (let ((buffer-file-name (buffer-local-value 'buffer-file-name buffer)))
                    (eglot-format-buffer))
                  (funcall callback)))
        '';

        usePackage = {
          eglot = {
            preface = "(defvar efs/autoformat t) ";
            gfhookf = [
              ''
                ('eglot-managed-mode (local! completion-at-point-functions (list (cape-capf-super #'tempel-complete
                	      #'eglot-completion-at-point
                											     #'cape-file))))
                	    ''
            ];
            config = ''
              	    (efs/evil-collection-remap 'evil-collection-eglot-setup 'normal eglot-mode-map
              	    			   "K" 'evil-substitute)
              	  '';
          };

          flymake-popon.setopt.flymake-popon-posframe-extra-arguments = [
            "':poshandler"
            "'posframe-poshandler-point-bottom-left-corner-upward"
            "':refposhandler"
            "'vertico-posframe-refposhandler-default"
          ];

          eglot-java = {
            setopt.eglot-java-user-init-opts-fn = "'eglot-java-init-opts";
            preface = ''
              	    (defun eglot-java-init-opts (server eglot-java-eclipse-jdt)
                                 '(:bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]))
              	  '';
          };

          citre = {
            enable = true;
            ghookf = [ "('prog-mode 'citre-mode)" ];
            gfhookf = [ "('doom-escape 'citre-peek-abort)" ];
            setopt = {
              citre-prompt-language-for-ctags-command = true;
              citre-use-project-root-when-creating-ctags = true;
              citre-ctags-program = ''"${pkgs.universal-ctags}/bin/ctags"'';
              citre-readtags-program = ''"${pkgs.universal-ctags}/bin/readtags"'';
            };
            custom.citre-peek-ace-keys = "'(?c ?r ?s ?t ?b ?f ?n ?e ?i ?a)";
            generalTwoConfig = {
              local-leader.citre-mode-map = {
                "p" = "'citre-ace-peek";
                "u" = "'citre-update-this-tags-file";
              };
              ":nvm".citre-peek-keymap = {
                "M-e" = "'citre-peek-next-line";
                "M-o" = "'citre-peek-prev-line";
              };
            };
            generalOneConfig.citre-peek-keymap = {
              "M-e" = "'citre-peek-next-line";
              "M-o" = "'citre-peek-prev-line";
            };
          };
        };
      };
    };
}
