{ pkgs, ... }:
{
  imports = [./dash.nix];

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

    direnv.enableBashIntegration = true;

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

    emacs.init = {
      completions.tempel.templates.eshell-mode.gbc = ''"(get-buffer-create \"" q "\")"'';
      terminals = {
	eshell = true;
	eat = true;
      };
      usePackage = {
        eshell = {
          ghookf = ["('eshell-first-time-mode 'efs/configure-eshell)"];
          general."s-<enter>" = "'efs/make-eshell";
          init = ''
            (defun efs/make-eshell ()
              (interactive)
              (eshell 'N))
          '';
          config = ''
            (defun efs/configure-eshell ()
              ;; Bind some useful keys for evil-mode
              (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
              (evil-normalize-keymaps)
              (gsetq eshell-command-aliases-list '(("gc" "torsocks git clone")
              				       ("nixbuild" "home-manager switch --flake ~/.config/home-manager/#holschcc")
              				       ("l" "ls $*")
              				       ("halt" "doas shutdown -P now")
            				       ("reboot" "doas reboot")
              				       ("systembuild" "doas nix run 'github:numtide/system-manager' -- switch --flake '/etc/system-manager/'"))))
            
            ;; https://xenodium.com/rinku-cli-link-previews
            (defun adviced:eshell/cat (orig-fun &rest args)
              "Like `eshell/cat' but with image support."
              (if (seq-every-p (lambda (arg)
                                 (and (stringp arg)
                                      (file-exists-p arg)
                                      (image-supported-file-p arg)))
                               args)
                  (with-temp-buffer
                    (insert "\n")
                    (dolist (path args)
                      (let ((newpath (expand-file-name path)))
            	    (insert-image (create-image
            			   newpath (image-type-from-file-name newpath)
            			   nil :max-width 350)))
                      (insert "\n"))
                    (insert "\n")
                    (buffer-string))
                (apply orig-fun args)))
            
            (advice-add #'eshell/cat :around #'adviced:eshell/cat)
          '';
        };
        
        fish-completion.gfhookf = ["('fish-completion-mode (local! completion-at-point-functions '(tempel-complete pcomplete-completions-at-point)))"];
        
        evil-collection-eshell = {
          enable = true;
          defer = true;
          generalTwoConfig.":n".eshell-mode-map = {
            "v" = "'evil-collection-eshell-evil-delete";
            "V" = "'evil-collection-eshell-evil-change";
            "C-v" = "'evil-collection-eshell-evil-delete-line";
          };
          config = ''
            (efs/evil-collection-remap 'evil-collection-eshell-setup-keys 'normal eshell-mode-map
            			   "d" 'evil-yank
            			   "D" 'evil-yank-line
            			   "c" 'evil-visual-state
            			   "C" 'evil-visual-line)
          '';
        };
      };
    };
  };
}
