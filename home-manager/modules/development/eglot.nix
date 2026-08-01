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
                					            (cape-capf-super
                                                                     #'tempel-complete
                                                                     (cape-capf-buster #'eglot-completion-at-point)
                                                                     (cape-capf-inside-comment #'cape-dict)
                                                                     (cape-capf-inside-string #'cape-dict)
                                                                     #'cape-dabbrev)))))
              ''
            ];
            config = ''
              (efs/evil-collection-remap
               'evil-collection-eglot-setup
               'normal
               eglot-mode-map
               "K"
               'evil-substitute)
            '';
          };

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
