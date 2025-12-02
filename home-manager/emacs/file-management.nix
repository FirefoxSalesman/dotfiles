{ inputs, pkgs, ... }:

{
  programs.emacs.init = {
    tools.dired = {
      enable = true;
      narrow = true;
      posframe = true;
    };
    usePackage = {
      dired = {
	generalOne.ctl-x-map."d" = "'consult-dir";
        # We're doing our best to get rid of that 1st extraneous line
	setopt.dired-free-space = false;
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
                        "${pkgs.mpvmacs}/bin/mpvmacs"
                        '(file))))
	'';
      };

      all-the-icons-dired = {
	enable = true;
	ghookf = ["('dired-mode 'all-the-icons-dired-mode)"];
      };

      dired-single = {
	enable = true;
	after = [ "dired" ];
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

      image = {
	enable = true;
	gfhookf = ["('image-mode 'image-transform-fit-to-window)"];
	generalTwoConfig.":n".image-map = {
          "E" = "'image-next-file";
          "O" = "'image-previous-file";
	};
	setopt.image-animate-loop = true;
      };
    } ;
  };
}
