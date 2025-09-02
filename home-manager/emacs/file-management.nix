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
      generalOne.global-leader."d" = '''(dired :which-key "dired")'';
      generalTwo.":n".dired-mode-map."w" = "'wdired-change-to-wdired-mode";
      custom = {
        dired-recursive-deletes = "'always";
        dired-listing-switches = ''"-agho --group-directories-first"'';
        # We're doing our best to get rid of that 1st extraneous line
        dired-free-space = false;
      };
      config = ''(with-eval-after-load 'dired-x (gsetq dired-omit-extensions (delete ".class" dired-omit-extensions)))'';
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
      extraConfig = '':general-config ('normal dired-mode-map "H" 'dired-hide-dotfiles-mode)'';
    };

    all-the-icons-dired = {
      enable = true;
      ghook = ["('dired-mode-hook 'all-the-icons-dired-mode)"];
    };

    dired-single = {
      enable = true;
      ghook = [''
          ('dired-mode-hook (lambda () (general-def 'normal dired-mode-map
               "B" 'evil-goto-line
               ;;"a" 'evil-ex
               ;;"o" 'evil-previous-line
               ;;"j" 'dired-unmark
               ;;"m" 'evil-search-next
               ;;"M" 'evil-search-previous
               "n" 'dired-single-prev
               "i" 'dired-single-next)))
        ''];
    };

    dired-ranger = {
      enable = true;
      ghook = [''
          ('dired-mode-hook (lambda () (general-def 'normal dired-mode-map
               "d" 'dired-ranger-copy
               "O" 'dired-ranger-move
               "G" 'dired-ranger-paste)))
        ''];
    };

    diredfl = {
      enable = true;
      ghook = ["('dired-mode-hook 'diredfl-mode)"];
    };

    dired-posframe = {
      enable = true;
      generalTwo.":n".dired-mode-map."M-t" = "'dired-posframe-mode";
    };
    
    image = {
      enable = true;
      gfhook = ["('image-mode-hook 'image-transform-fit-to-window)"];
      generalTwo.":n".image-map = {
        "E" = "'image-next-file";
        "O" = "'image-previous-file";
      };
      custom.image-animate-loop = true;
    };

    dired-narrow = {
      enable = true;
      generalTwo.":n".dired-mode-map."N" = "'dired-narrow-fuzzy";
    };
  };
}
