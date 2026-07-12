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
        # narrow = true;
        # posframe = true;
        async = true;
      };
      usePackage = {
        nerd-icons-dired.enable = lib.mkForce false;

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
          enable = true;
          extraPackages = with pkgs; [
            vips
            mediainfo
          ];
          afterCall = [ "on-first-input-hook" ];
          generalOne.global-leader."T" = "'dirvish-side";
          # Borrowed from doom
          generalTwoConfig.":n".dirvish-mode-map = {
            "n" = "'dired-up-directory";
            "i" = "'dired-find-file";
            "N" = "'dirvish-narrow";
            "M-m" = "'dirvish-mark-menu";
            "B" = "'evil-goto-line";
            "f" = "'dirvish-file-info-menu";
            "G" = "'dirvish-yank";
            "S" = "'dirvish-quicksort";
            "F" = "'dirvish-layout-toggle";
            "q" = "'dirvish-history-jump";
            "bn" = "'dirvish-subtree-up";
            "bi" = "'dirvish-subtree-toggle";
            "C-i" = "'dirvish-subtree-toggle";
            "[h" = "'dirvish-history-go-backward";
            "]h" = "'dirvish-history-go-forward";
            "[e" = "'dirvish-emerge-next-group";
            "]e" = "'dirvish-emerge-previous-group";
            "M-e" = "'dirvish-emerge-menu";
            "g" = '''(:ignore t :which-key "yank")'';
            "gl" = "'dirvish-copy-file-true-path";
            "gn" = "'dirvish-copy-file-name";
            "gp" = "'dirvish-copy-file-path";
            "s" = '''(:ignore t :which-key "symlinks")'';
            "ss" = "'dirvish-symlink";
            "sS" = "'dirvish-relative-symlink";
            "sh" = "'dirvish-hardlink";
            "O" = "'dirvish-move";
          };
          # Borrowed from doom
          setopt =
            let
              dirvishTypes = [
                "'dirvish"
                "'dirvish-side"
              ];
            in
            {
              dirvish-reuse-session = "'open";
              dirvish-attributes = [
                "'file-size"
                "'nerd-icons"
                "'subtree-state"
              ];
              dirvish-hide-details = dirvishTypes;
              dirvish-hide-cursor = dirvishTypes;
              dirvish-use-mode-line = false;
              dirvish-preview-disabled-exts = [
                ''"bin"''
                ''"exe"''
                ''"gpg"''
                ''"elc"''
                ''"eln"''
                ''"xcf"''
                ''"odt"''
                ''"doc"''
                ''"docx"''
                ''"odp"''
                ''"pptx"''
                ''"xlsx"''
              ];
            };
          config = ''
            (dirvish-override-dired-mode)
            (advice-add #'dired--find-file :override #'dirvish--find-entry)
            (advice-add #'dired-noselect :around #'dirvish-dired-noselect-a)
            (advice-add #'dirvish-side :after (local! window-size-fixed t))
            
            (add-to-list
             #'golden-ratio-inhibit-functions
             (lambda ()
               (let ((matches (lambda (prefix) (string-prefix-p prefix (buffer-name (current-buffer))))))
                 (or (funcall matches " *SIDE :: ")
            	 (funcall matches "*dirvish-parent-1")))))
            
            (with-eval-after-load 'dirvish-yank
              (defun dirvish-yank--apply (method dest)
                "Apply yank METHOD to DEST."
                (setq dest (expand-file-name (or dest (dired-current-directory))))
                (let ((srcs
                       (or (and (not
                                 (member
                                  dirvish-yank-sources '(all session buffer)))
                                (functionp dirvish-yank-sources)
                                (funcall dirvish-yank-sources))
                           (dirvish-yank--get-srcs dirvish-yank-sources)
                           (user-error "DIRVISH[yank]: no marked files"))))
                  (dirvish-yank-default-handler method srcs dest))))
            
            (dirvish-peek-mode)
          '';
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
