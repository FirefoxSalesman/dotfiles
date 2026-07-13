{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.mpvmacs = pkgs.writeShellScriptBin "mpvmacs" ''
        emacsclient -e "(mpv-play \"$1\")"
      '';
    };

  flake.homeModules.fileManager = { pkgs, lib, ... }: {
    programs.emacs.init = {
      tools.dired = {
        enable = true;
        narrow = true;
        dirvish = {
          enable = true;
          previews = true;
        };
        async = true;
      };
      usePackage = {
        dired = {
          generalOne.ctl-x-map."d" = "'consult-dir";
          setopt = {
            dired-free-space = false;
            # Borrowed from Doom
            image-dired-thumb-size = 150;
          };
          config = ''
            (with-eval-after-load 'dired-x
              (gsetq
               dired-omit-extensions (delete ".class" dired-omit-extensions)))
          '';
        };

        dirvish = {
          gfhookf = ["('dirvish-directory-view-mode 'diredfl-mode)"];
          setopt.dirvish-use-mode-line = false;
          # Borrowed from doom
          generalTwoConfig.":n".dirvish-mode-map = {
            "n" = "'dired-up-directory";
            "i" = "'dired-find-file";
            "B" = "'evil-goto-line";
            "G" = "'dirvish-yank";
            "q" = "'dirvish-history-jump";
            "bn" = "'dirvish-subtree-up";
            "bi" = "'dirvish-subtree-toggle";
            "g" = '''(:ignore t :which-key "yank")'';
            "gl" = "'dirvish-copy-file-true-path";
            "gn" = "'dirvish-copy-file-name";
            "gp" = "'dirvish-copy-file-path";
            "O" = "'dirvish-move";
          };
        };

        openwith = {
          enable = true;
          defer = true;
          ghookf = [ "('dired-mode 'openwith-mode)" ];
          config = ''
            (gsetq
             openwith-associations
             (list
              (list (openwith-make-extension-regexp '("xcf")) "gimp" '(file))
              (list
               (openwith-make-extension-regexp
                '("odt" "doc" "docx" "odp" "pptx" "xlsx"))
               "libreoffice" '(file))
              (list
               (openwith-make-extension-regexp
                '("mpg"
                  "mpeg"
                  "mp3"
                  "mp4"
                  "avi"
                  "wmv"
                  "wav"
                  "mov"
                  "flv"
                  "ogm"
                  "ogg"
                  "mkv"
                  "webm"
                  "aac"
                  "ape"
                  "opus"
                  "flac"))
               "${lib.getExe pkgs.mpvmacs}" '(file))))
          '';
        };

        image = {
          enable = true;
          gfhookf = [ "('image-mode 'image-transform-fit-to-window)" ];
          generalTwoConfig.":n".image-map = {
            "E" = "'image-next-file";
            "O" = "'image-previous-file";
          };
          setopt.image-animate-loop = true;
        };
      };
    };
  };
}
