{
  flake.homeModules.development =
    { pkgs, ... }:
    {
      programs.emacs.init = {
        ide = {
          magit = {
            enable = true;
            forge = true;
            todo = true;
          };
          project = true;
        };
        usePackage = {
          magit = {
            setopt.magit-process-find-password-functions = [ "'magit-process-password-auth-source" ];
            generalOneConfig = {
              magit-mode-map = {
                "e" = "'evil-next-visual-line";
                "B" = "'evil-goto-line";
              };
              magit-status-mode-map."j" = "'magit-unstage-files";
            };
            # https://github.com/magit/magit/issues/5557
            config = ''
                (defalias 'magit--any
                        (static-if (fboundp 'member-if) #'member-if #'cl-member-if))
              '';
          };

          majutsu = {
            enable = true;
	    extraPackages = [pkgs.jujutsu];
	    command = ["majutsu"];
          };

          projection-ibuffer = {
            enable = true;
            generalOne.project-prefix-map = {
              C-b = ''`("ibuffer" . ,(cmd! (ibuffer) (ibuffer-filter-by-projection-root (project-current))))'';
              i = '''("info" . projection-show-project-info)'';
            };
          };

          projection-multi = {
            custom.projection-gradle-use-daemon = false;
            config = ''
              (advice-add
               'projection-multi-npm-scripts--targets-from-file2
               :override
               (lambda ()
                 (hash-table-keys
                  (gethash
                   "scripts"
                   (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name
                       (concat (project-root (project-current)) "package.json")))
                     (json-parse-string (buffer-string)))))))
            '';
          };
        };
      };
    };
}
