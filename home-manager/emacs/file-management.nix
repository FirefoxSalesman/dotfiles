{ inputs, ... }:

{
  programs.emacs.init.usePackage = {
    dired = {
      enable = true;
      gfhook = ["('dired-mode-hook (list 'dired-omit-mode 'hl-line-mode (lambda () (setq-local
      visible-cursor nil))))"];
      general = {
        "C-x C-j" = "'dired-jump";
        "C-x d" = "'consult-dir";
      };
      generalOne."efs/leader-keys"."d" = '''(dired :which-key "dired")'';
      generalTwo."'normal".dired-mode-map."w" = "'wdired-change-to-wdired-mode";
      custom = {
        dired-recursive-deletes = "'always";
        dired-listing-switches = ''"-agho --group-directories-first"'';
        # We're doing our best to get rid of that 1st extraneous line
        dired-free-space = "nil";
      };
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

    all-the-icons-dired = {
      enable = true;
      ghook = ["('dired-mode-hook 'all-the-icons-dired-mode)"];
    };

    dired-single = {
      enable = true;
      package = epkgs: (epkgs.callPackage ./emacs-packages/dired-single.nix {
        inherit inputs;
        inherit (epkgs) trivialBuild;
      });
      ghook = [''
        ('dired-mode-hook (lambda () (general-def 'normal dired-mode-map
             "B" 'evil-goto-line
             "n" 'dired-single-prev
             "i" 'dired-single-next)))
      ''];
    };

    diredfl = {
      enable = true;
      ghook = ["('dired-mode-hook 'diredfl-mode)"];
    };

    dired-posframe = {
      enable = true;
      generalTwo."'normal".dired-mode-map."M-t" = "'dired-posframe-mode";
    };
    
    image = {
      enable = true;
      generalTwo."'normal".image-map = {
        "E" = "'image-next-file";
        "O" = "'image-previous-file";
      };
      custom.image-animate-loop = "t";
    };
  };
}
