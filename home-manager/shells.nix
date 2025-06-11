{ pkgs, ... }:
{
  imports = [
        ./dash.nix
  ];

  home.shellAliases = {
    ffrecord = "ffmpeg -f sndio -i snd/0.mon -f x11grab -r 30 -s 1920x1080 -i :0 -c:v libx164 -preset ultrafast -acodec copy ~/test.mkv";
    ffaud = "ffmpeg -f alsa -channels 1 -sample_rate 44100 -i default:CARD=Mic output.flac";
    sx = "startx";
    otp = "pass otp";
    run = "cd /run/";
    l = "ls";
    ".." = "cd ..";
    tortube = "yt-dlp --proxy socks://localhost:9050";
    wget = "torsocks wget --hsts-file=$XDG_DATA_HOME/wget-hsts";
    ga = "git add";
    gc = "torsocks git clone";
    gp = "git pull";
    gP = "git push";
  };

  programs = {
    fish = {
      enable = true;
      package = pkgs.fish;
      interactiveShellInit = ''${pkgs.pfetch}/bin/pfetch'';
    };

    bash = {
      enable = true;
      initExtra = ''${pkgs.pfetch}/bin/pfetch'';
      shellAliases.z = "cd ./$(ls -d */ .*/ | ${pkgs.ezf}/bin/ezf)";
    };

    starship = {
      enable = true;
      enableFishIntegration = true;
      enableBashIntegration = true;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };
    
    emacs.init.usePackage.envrc = {
        enable = true;
        ghook = ["('after-init-hook 'envrc-global-mode)"];
        generalOne."efs/leader-keys" = {
          "e" = '''(:ignore t :which-key "direnv")'';
          "ea" = '''(envrc-allow :which-key "allow")'';
          "eu" = '''(envrc-reload :which-key "update dir")'';
        };
    };

    dash = {
      enable = true;
      initExtra = ''
        ${pkgs.pfetch}/bin/pfetch
        . ~/.cache/wal/colors.sh
      '';
      shellAliases.z = "cd ./$(ls -d */ .*/ | ${pkgs.ezf}/bin/ezf)";
      profileExtra = ''
        export LEIN_HOME="$XDG_DATA_HOME/lein";
        export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc";
        export XCOMPOSECACHE="$XDG_CACHE_HOME/x11/xcompose";
        export GOPATH="$XDG_DATA_HOME/go";
        export DOT_SAGE="$XDG_CONFIG_HOME/sage";
        # export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority";
        export CARGO_HOME="$XDG_DATA_HOME/cargo";
        export GNUPGHOME="$XDG_DATA_HOME/share/gnupg";
        export PASSWORD_STORE_DIR="$XDG_DATA_HOME/share/pass";
        export CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv";
        export ERRFILE="$XDG_CACHE_HOME/X11/xsession-errors"
        export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
        export HISTFILE="''${XDG_STATE_HOME}"/bash/history
        GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle
        
        [ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx
      '';
    };

    emacs.init.usePackage = {
        eshell = {
          enable = true;
          after = ["evil-collection"];
          ghook = [
            "('eshell-first-time-mode-hook 'efs/configure-eshell)"
            #Save command history when commands are entered
            "('eshell-precommand-hook 'eshell-save-some-history)"
            #pfetch
            ''('eshell-banner-load-hook  (lambda ()
                                           (gsetq eshell-banner-message
                                              (shell-command-to-string "${pkgs.pfetch}/bin/pfetch"))))''
          ];
          general."s-<enter>" = "'efs/make-eshell";
          generalOne.eshell-mode-map = {
            "M-o" = "'eshell-previous-matching-input-from-input";
            "M-e" = "'eshell-next-matching-input-from-input";
          };
          generalTwo.local-leader.eshell-mode-map = {
            "e" = '''(eshell-insert-envvar :which-key "insert environment variable")'';
            "b" = '''(eshell-insert-buffer-name :which-key "insert buffer name")'';
          };
          init = ''
        (defun efs/make-eshell ()
          (interactive)
          (eshell 'N))
      '';
          config = ''
        (defun efs/configure-eshell ()
          ;; Truncate buffer for perforance
          (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
        
          ;; Bind some useful keys for evil-mode
          (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
          (evil-normalize-keymaps)
          (setenv "TERM" "xterm")
          (gsetq eshell-command-aliases-list '(("gc" "torsocks git clone")
        				       ("nixbuild" "home-manager switch --flake ~/.config/home-manager/#holschcc")
        				       ("l" "ls $*")
        				       ("halt" "doas shutdown -P now")
        				       ("systembuild" "doas nix run 'github:numtide/system-manager' -- switch --flake '/etc/system-manager/'")
        				       ("trash" "rm -rf ~/.local/Trash"))
        	 eshell-history-size 0
                 eshell-buffer-maximum-lines 100
                 eshell-hist-ignoredups t
                 eshell-scroll-to-bottom-on-input t))
        
        (with-eval-after-load 'esh-opt
          (gsetq eshell-destroy-buffer-when-process-dies t))
        
        (with-eval-after-load 'evil-collection-eshell
          (general-add-advice 'evil-collection-eshell-setup-keys
        		:after
        		'(lambda ()
        		   (general-def 'normal eshell-mode-map
        		     "v" 'evil-collection-eshell-evil-delete
        		     "V" 'evil-collection-eshell-evil-change
        		     "C-v" 'evil-collection-eshell-evil-delete-line
        		     "d" 'evil-yank
        		     "D" 'evil-yank-line
        		     "c" 'evil-visual-state
        		     "C" 'evil-visual-line))))
      '';
        } ;
      
        eshell-syntax-highlighting = {
          enable = true;
          defer = true;
          ghook = ["('eshell-mode-hook 'eshell-syntax-highlighting-global-mode)"];
        };
      
        fish-completion = {
          enable = true;
          defer = true;
          ghook = ["('eshell-mode-hook 'fish-completion-mode)"];
        };
      
        eshell-git-prompt = {
          enable = true;
          afterCall = ["eshell-mode"];
          config = ''(eshell-git-prompt-use-theme 'powerline)'';
        };
      
        eat = {
          enable = true;
          defer = true;
          afterCall = ["eshell-mode"];
          config = ''
            (eat-eshell-mode)
            (evil-ex-define-cmd "term" 'eat)
            (defun eat-term-get-suitable-term-name (&optional display)
              "Return the most suitable value for `TERM' for DISPLAY.
            
              If the number of colors supported by display (as returned by
              `display-color-cells') is more than 256, return \"eat-truecolor\", if
              it is more than 8 but less than or equal to 256, return
              \"eat-256color\", if is more than 1 but less than or equal to 8,
              return \"eat-color\", otherwise return \"eat-mono\"."
              (let ((colors (display-color-cells display)))
                (cond ((> colors 256) "xterm")
                      ((> colors 8) "xterm")
                      ((> colors 1) "xterm")
                      (t "xterm"))))
            
          '';
        };
    };
  };
}
