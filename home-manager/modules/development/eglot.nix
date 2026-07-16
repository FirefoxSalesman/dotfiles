{
  flake.homeModules.development =
    {
      pkgs,
      lib,
      config,
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
                ('eglot-managed-mode (local! completion-at-point-functions (list (cape-capf-super #'tempel-complete
                	      #'eglot-completion-at-point
                											     #'cape-file))))
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
        };
      };
    };
}
