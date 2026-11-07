{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.code-review = epkgs.callPackage epkgs.trivialBuild rec {
        pname = "code-review";
        version = "pinned";
        src = inputs.code-review;
        propagatedUserEnvPkgs = with epkgs; [
          closql
          magit
          transient
          a
          ghub
          uuidgen
          deferred
          markdown-mode
          forge
          emojify
        ];
        buildInputs = propagatedUserEnvPkgs;
      };
    };

  flake.homeModules.development =
    { pkgs, lib, ... }:
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
          git-modes = {
            enable = true;
            mode = [
              ''("/\\.gitattributes\\'" . gitattributes-mode)''
              ''("/info/attributes\\'" . gitattributes-mode)''
              ''("/git/attributes\\'" . gitattributes-mode)''
              ''("/\\.gitconfig\\'" . gitconfig-mode)''
              ''("/\\.git/config\\'" . gitconfig-mode)''
              ''("/modules/.*/config\\'" . gitconfig-mode)''
              ''("/git/config\\'" . gitconfig-mode)''
              ''("/\\.gitmodules\\'" . gitconfig-mode)''
              ''("/etc/gitconfig\\'" . gitconfig-mode)''
              ''("/\\.gitignore\\'" . gitignore-mode)''
              ''("/info/exclude\\'" . gitignore-mode)''
              ''("/git/ignore\\'" . gitignore-mode)''
            ];
          };

          magit = {
            setopt.magit-process-find-password-functions = [ "'magit-process-password-auth-source" ];
            generalOne.project-prefix-map =
              let
                mkMajutsuCmd = magitCommand: majutsuCommand: ''
                  `,(cmd! (if (file-exists-p (nix-emacs-project-file ".jj"))
                          (progn ${majutsuCommand})
                          ${magitCommand}))
                '';
              in
              {
                "v" = mkMajutsuCmd "(magit)" "(majutsu)";
                "c" = mkMajutsuCmd "(magit-commit)" "(majutsu-bookmark-advance) (majutsu-commit)";
                "p" = mkMajutsuCmd "(magit-pull)" "(call-interactively 'majutsu-git-fetch) (majutsu-rebase)";
                "P" = mkMajutsuCmd "(magit-push)" "(call-interactively 'majutsu-git-push)";
                "b" = mkMajutsuCmd "(magit-branch)" "(majutsu-bookmark)";
              };
            bindLocal.project-prefix-map = lib.mkForce { };
            generalOneConfig = {
              magit-mode-map = {
                "e" = "'evil-next-visual-line";
                "B" = "'evil-goto-line";
              };
              magit-status-mode-map."j" = "'magit-unstage-files";
            };
          };

          code-review.enable = lib.mkForce false;

          majutsu = {
            enable = true;
            extraPackages = [ pkgs.jujutsu ];
            command = [ "majutsu-git-fetch" ];
            extraConfig = ":autoload majutsu-repository-config-id";
            config = ''
              	      (magit-add-section-hook 'majutsu-log-sections-hook #'magit-todos--insert-todos nil 'append)
              	      (majutsu-forge-mode)
              	    '';
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
