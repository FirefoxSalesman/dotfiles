{
  flake.homeModules.development =
    {
      pkgs,
      lib,
      ...
    }:
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

        usePackage = {
          eglot = {
            gfhookf = [
              ''
                ('eglot-managed-mode (local! completion-at-point-functions
                                                             (list (cape-capf-choose
                                                                    #'efs/mixin-completion-at-point
                					            (cape-capf-super
                                                                     #'tempel-complete
                                                                     (cape-capf-buster #'eglot-completion-at-point)
                                                                     (cape-capf-inside-comment #'cape-dict)
                                                                     (cape-capf-inside-string #'cape-dict))
                                                                    #'cape-dabbrev))))
              ''
            ];
            # https://www.jamescherti.com/emacs-eglot-performance/
            setopt = {
              eglot-sync-connect = lib.mkForce 0;
              eglot-events-buffer-config = [
                "':size"
                0
                "':format"
                "'full"
              ];
              eglot-max-file-watches = 3000;
              eglot-report-progress = false;
              eglot-code-action-indications = false;
            };
            config = ''
              (efs/evil-collection-remap
               'evil-collection-eglot-setup
               'normal
               eglot-mode-map
               "K"
               'evil-substitute)
              
              (add-to-list 'eglot-ignored-server-capabilities :foldingRangeProvider)
            '';
          };

	  #https://github.com/nemethf/eglot-x/blob/5cd6f936b9dc571edd4437b8fbf93ff68b0e723b/eglot-x.el
          eglot-x.config = ''
            (cl-defmethod eglot-execute :around (server action)
              "Execute ACTION locally if possible, otherwise ask SERVER to execute it."
              (if (not eglot-x-client-commands)
                  (cl-call-next-method)
                ;; This is almost the same as the upstream `eglot-execute'.
                (eglot--dcase action
                  (((Command)) (eglot-x-execute-command server action))
                  (((CodeAction) edit command data)
                   (if (and (null edit) (null command) data
                            (eglot-server-capable :codeActionProvider :resolveProvider))
                       (eglot-execute server
                                      (eglot--request server :codeAction/resolve action))
                     (when edit (eglot--apply-workspace-edit server edit this-command))
                     (when command
                       (eglot-x-execute-command server command)))))))
          '';

          flymake.setopt.flymake-show-diagnostics-at-end-of-line = "'short";
          flymake-popon.enable = lib.mkForce false;

          eglot-java = {
            setopt.eglot-java-user-init-opts-fn = "'eglot-java-init-opts";
            preface = ''
              (defun eglot-java-init-opts (server eglot-java-eclipse-jdt)
                                 '(:bundles ["/usr/share/java-debug/com.microsoft.java.debug.plugin.jar"]))
            '';
          };
        };
      };
    };
}
