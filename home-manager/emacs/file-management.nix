{ inputs, ... }:

{
  programs.emacs.init.usePackage = {
    dired = {
      enable = true;
      gfhook = ["('dired-mode-hook 'dired-omit-mode)"];
      bind = {
        "C-x C-j" = "dired-jump";
        "C-x d" = "consult-dir";
      };
      custom = {
        dired-recursive-deletes = "'always";
        dired-listing-switches = ''"-agho --group-directories-first"'';
      };
      generalOne."efs/leader-keys"."d" = '''(dired :which-key "dired")'';
    };

    openwith = {
      enable = true;
      defer = true;
      ghook = ["('dired-mode-hook 'openwith-mode)"];
      config = ''
    (gsetq openwith-associations
         (list
          (list (openwith-make-extension-regexp
                 '("ff"))
                "lel"
                '(file))
          (list (openwith-make-extension-regexp
                 '("odt" "doc" "docx" "odp" "pptx" "xlsx"))
                "libreoffice"
                '(file))
          (list (openwith-make-extension-regexp
                 '("mpg" "mpeg" "mp3" "mp4"
                   "avi" "wmv" "wav" "mov" "flv"
                   "ogm" "ogg" "mkv" "webm" "opus"
                   "flac"))
                "mpv"
                '(file))))
    
    (defun dired-do-async-delete (&optional arg)
      "Delete all marked (or next ARG) files.
    `dired-recursive-deletes' controls whether deletion of
    non-empty directories is allowed."
      ;; This is more consistent with the file marking feature than
      ;; dired-do-flagged-delete.
      (interactive "P")
      (let (markers)
        (dired-internal-do-deletions
         (nreverse
          ;; this may move point if ARG is an integer
          (dired-map-over-marks (cons (dired-get-filename)
                                      (let ((m (point-marker)))
                                        (push m markers)
                                        m))
                                arg))
         arg t)
        (async-start (lambda ()
      		 (dolist (m markers) (set-marker m nil)))
      	       'ignore)))
  '';
    };

    dired-hide-dotfiles = {
      enable = true;
      defer = true;
      ghook = ["('dired-mode-hook 'dired-hide-dotfiles-mode)"];
      config = ''(general-def 'normal dired-mode-map "H" 'dired-hide-dotfiles-mode)'';
    };

    dirvish = {
      enable = true;
      bind."H-f" = "dirvish-fd";
      custom = {
        dirvish-attributes = "'(nerd-icons file-size subtree-state)";
        dirvish-yank-overwrite-existing-files = "'always";
        dirvish-side-window-parameters = "nil";
        dirvish-quick--entries = ''
       '(("h" "~/" "Home"
          "r" "~/game/rejuvenation/"))
    '';
      };
      afterCall = ["on-first-buffer-hook"];
      generalTwo = {
        local-leader.dirvish-mode-map."s" = "'dirvish-setup-menu";
        "'normal".dirvish-mode-map = {
          "d" = "'dired-do-async-delete";
          "/" = "'consult-line";
          "c" = "'dirvish-quick-access";
          "w" = "'wdired-change-to-wdired-mode";
          "q" = "'evil-record-macro";
          "m" = "'evil-search-next";
          "I" = "'dirvish-file-info-menu";
          "y" = "'dirvish-yank-menu";
          "N" = "'dirvish-narrow";
          "B" = "'evil-goto-line";
          "^" = "'dirvish-history-last";
          "u" = "'dirvish-history-jump";
          "j" = "'dired-unmark";
          "s" = "'dirvish-quicksort ;remapped 'dired-sort-toggle-or-edit'";
          "v" = "'dirvish-vc-menu ;remapped 'dired-view-file'";
          "TAB" = "'dirvish-subtree-toggle";
          "M-f" = "'dirvish-history-go-forward";
          "M-b" = "'dirvish-history-go-backward";
          "M-l" = "'dirvish-ls-switches-menu";
          "M-m" = "'dirvish-mark-menu";
          "M-E" = "'dirvish-emerge-menu";
          "M-j" = "'dirvish-fd-jump";
        };
      };
      generalOne."efs/leader-keys"."t" = '''(dirvish-side :which-key "side-bar")'';
      config = ''
    (dirvish-override-dired-mode)
    (dirvish-peek-mode)
    (require 'dirvish-yank)
  '';
    };

    dired-single = {
      enable = true;
      package = epkgs: (epkgs.callPackage ./emacs-packages/dired-single.nix {
        inherit inputs;
        inherit (epkgs) trivialBuild;
      });
      generalTwo = {
        "'normal".dirvish-mode-map = {
          "i" = "'dired-single-prev";
          "n" = "'dired-single-next";
        };
      };
    };

    diredfl = {
      enable = true;
      ghook = ["('dired-mode-hook 'diredfl-mode)"];
    };

    dired-posframe = {
      enable = true;
      generalTwo."'normal".dirvish-mode-map."M-t" = "'dired-posframe-mode";
    };
  };
}
