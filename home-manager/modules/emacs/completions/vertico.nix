{
  flake.homeModules.emacs.programs.emacs.init = {
    completions = {
      vertico.enable = true;
      prescient = true;
      orderless = true;
    };
    usePackage = {
      vertico = {
        generalTwoConfig.":n" = {
          vertico-grid-map = {
            "i" = "'vertico-grid-right";
            "n" = "'vertico-grid-left";
            "C-o" = "'vertico-grid-scroll-down";
            "C-e" = "'vertico-grid-scroll-up";
          };
          vertico-map = {
            "C-o" = "'vertico-scroll-down";
            "C-e" = "'vertico-scroll-up";
            "j" = "'evil-undo";
            "I" = "'vertico-last";
            "N" = "'vertico-first";
            "B" = "'vertico-last";
            "bg" = "'vertico-first";
            "G" = "'evil-paste-after";
          };
        };
        config = ''
          (with-eval-after-load 'evil-collection-vertico
            (efs/evil-collection-remap
             'evil-collection-vertico-setup
             'normal
             vertico-map
             "k"
             'evil-delete-char))
          
          (defun vertico--format-count ()
            "Format the count string."
            (concat
             (propertize ""
                         'face
                         `(:foreground
                           ,(face-foreground 'default nil 'default)
                           :background ,(face-background 'default nil 'default)
                           :weight
                           ,(face-attribute 'bold :weight nil 'default)))
             (propertize (format (car vertico-count-format)
                                 (format (cdr vertico-count-format)
                                         (cond
                                          ((>= vertico--index 0)
                                           (1+ vertico--index))
                                          (vertico--allow-prompt
                                           "*")
                                          (t
                                           "!"))
                                         vertico--total))
                         'face
                         `(:foreground
                           ,(face-background 'default nil 'default)
                           :background ,(ewal-get-color 'yellow)
                           :weight
                           ,(face-attribute 'bold :weight nil 'default)))
             (propertize ""
                         'face
                         `(:foreground
                           ,(ewal-get-color 'yellow)
                           :background
                           ,(face-background 'widget-field nil 'default)
                           :weight
                           ,(face-attribute 'bold :weight nil 'default)))
             " "))
        '';
      };

      vertico-multiform = {
        enable = true;
        setopt.vertico-multiform-commands = [
          "'(music-menu grid)"
          "'(man grid)"
          # "'(t posframe)"
        ];
        ghookf = [ "('vertico-mode 'vertico-multiform-mode)" ];
      };

      vertico-quick.setopt = {
        vertico-quick1 = ''"crstbfneia"'';
        vertico-quick2 = ''"crstbfneia"'';
      };
    };
  };
}
