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
	    # https://github.com/yveszoundi/eglot-java/pull/68/files
            config = lib.mkForce ''
              (defun eglot-java--jdt-uri-handler (_operation &rest args)
                "Support Eclipse jdtls `jdt://' uri scheme."
                (let* ((uri (car args))
                       (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
                       (source-file
                        (expand-file-name
                         (eglot-java--make-path
                          cache-dir
                          (save-match-data
                            (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.\\(java\\|class\\)\\?" uri)
                              (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
                  (unless (file-readable-p source-file)
                    (let ((content (jsonrpc-request (eglot-java--find-server) :java/classFileContents (list :uri uri)))
                          (metadata-file (format "%s.%s.metadata"
                                                 (file-name-directory source-file)
                                                 (file-name-base source-file))))
                      (unless (file-directory-p cache-dir) (make-directory cache-dir t))
                      (with-temp-file source-file (insert content))
                      (with-temp-file metadata-file (insert uri))))
                  source-file))
            '';
            hook = [ "(java-ts-mode . eglot-java-mode)" ];
          };
        };
      };
    };
}
