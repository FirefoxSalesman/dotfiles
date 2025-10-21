{ inputs, ... }:

{
  programs.emacs.init.usePackage = {
    dired = {
      enable = true;
      gfhookf = ["('dired-mode (list 'dired-omit-mode 'hl-line-mode (local!
        visible-cursor nil)))"];
      general = {
        "C-x C-j" = "'dired-jump";
        "C-x d" = "'consult-dir";
      };
      generalOne.global-leader."d" = '''("dired" . dired)'';
      generalTwoConfig.":n".dired-mode-map."w" = "'wdired-change-to-wdired-mode";
      setopt = {
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
      ghookf = ["('dired-mode 'openwith-mode)"];
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
        '';
    };

    dired-hide-dotfiles = {
      enable = true;
      defer = true;
      ghookf = ["('dired-mode 'dired-hide-dotfiles-mode)"];
      generalTwoConfig.":n".dired-mode-map."H" = "'dired-hide-dotfiles-mode";
    };

    all-the-icons-dired = {
      enable = true;
      ghookf = ["('dired-mode 'all-the-icons-dired-mode)"];
    };

    dired-single = {
      enable = true;
      ghookf = [''
          ('dired-mode (lambda () (general-def 'normal dired-mode-map
               "B" 'evil-goto-line
               "n" 'dired-single-prev
               "i" 'dired-single-next)))
        ''];
    };

    dired-ranger = {
      enable = true;
      ghookf = [''
          ('dired-mode (lambda () (general-def 'normal dired-mode-map
               "d" 'dired-ranger-copy
               "O" 'dired-ranger-move
               "G" 'dired-ranger-paste)))
        ''];
    };

    diredfl = {
      enable = true;
      ghookf = ["('dired-mode 'diredfl-mode)"];
    };

    dired-posframe = {
      enable = true;
      generalTwo.":n".dired-mode-map."M-t" = "'dired-posframe-mode";
    };
    
    image = {
      enable = true;
      gfhookf = ["('image-mode 'image-transform-fit-to-window)"];
      generalTwoConfig.":n".image-map = {
        "E" = "'image-next-file";
        "O" = "'image-previous-file";
      };
      setopt.image-animate-loop = true;
    };

    dired-narrow = {
      enable = true;
      generalTwo.":n".dired-mode-map."N" = "'dired-narrow-fuzzy";
    };
  };
}
