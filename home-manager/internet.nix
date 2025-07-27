{ inputs, config, pkgs, pkgs-stable, lib, ... }:

{
  home.packages = with pkgs; [
    pkgs-stable.python39Packages.adblock
  ];

  programs.emacs.init.usePackage = {
    browse-url = {
      enable = true;
      config = "(defun qutebrowser-browse-url (url &rest args) (qutebrowser-open-url url 'tab))";
      custom = {
        browse-url-handlers = ''
          '(("https:\\/\\/www\\.youtu\\.*be." . efs/mpv-browse-url)
            ("https:\\/\\/yewtu\\.*be." . efs/mpv-browse-url)
            ("https:\\/\\/inv\\.*nadeko\\.*net\\/watch." . efs/mpv-browse-url)
            ("search\\.nixos\\.org[^z-a]*" . qutebrowser-browse-url)
            ("melpa\.org\.*" . qutebrowser-browse-url)
            ("." . (lambda (url &rest args) (eww url (prefix-numeric-value 4)))))
        '';
        browse-url-secondary-browser-function = "'browse-url-default-browser";
      };
    };
    

    eww = {
      enable = true;
      custom = {
        eww-search-prefix = ''"https://search.inetol.net/?q="'';
        eww-header-line-format = "nil";
        eww-desktop-remove-duplicates = "t";
        eww-download-directory = ''(expand-file-name "~/dwn")'';
        eww-history-limit = "10";
        # External Browser
        eww-use-external-browser-for-content-type = ''"\\`\\(video/\\|audio\\)"''; # On GNU/Linux check your mimeapps.list
        eww-browse-url-new-windowis-tab = "nil";
        eww-form-checkbox-selected-symbol = ''"[X]"'';
        eww-form-checkbox-symbol = ''"[ ]"'';
        eww-auto-rename-buffer = "'title";
      };
      generalTwo."'normal".eww-mode-map = {
        "N" = "#'eww-back-url";
        "I" = "#'eww-forward-url";
        "P" = "#'eww-copy-page-url";
        "R" = "'eww-readable";
      };
      init = ''
        (with-eval-after-load 'evil-collection-eww
          (general-add-advice 'evil-collection-eww-setup
        		      :after
        		      '(lambda ()
        			 (general-def 'normal eww-mode-map
        			   "d" 'evil-yank
        			   "p" 'efs/mpv-eww-url))))
      '';
    };

    yeetube = {
      enable = true;
      generalOne."efs/leader-keys"."y" = '''(yeetube-search :which-key "search")'';
      generalTwo."'normal"."yeetube-mode-map" = {
        "RET" = "'yeetube-play";
        "r" = "'yeetube-channel-videos";
        "s" = "'yeetube-channel-search";
      };
    };

    # ement = {
    #   enable = true;
    #   defer = true;
    #   generalOne = {
    #     "efs/leader-keys"."e" = '''(ement-connect :which-key "element")''; 
    #     local-leader."s" = '''(ement-sidebar :which-key "sidebar")'';
    #   };
    #   init = ''
    #     (defun ement-sidebar ()
    #       (interactive)
    #       (ement-room-list-side-window)
    #       (with-selected-window (get-buffer-window "*Ement Room List*")
    #         (gsetq window-size-fixed 'width)
    #         (window-resize (selected-window) (- 45 (window-total-width)) t t)))
    #   '';
    # };

    webjump = {
      enable = true;
      generalOne."efs/leader-keys"."s" = "'webjump";
      custom.webjump-sites = ''
        '(("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
          ("Invidious" . [simple-query "inv.nadeko.net" "inv.nadeko.net/search?q=" ""])
          ("Aur" . [simple-query "aur.archlinux.org" "aur.archlinux.org/packages/?K=" ""])
          ("Nixpkgs" . [simple-query "search.nixos.org" "search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=" ""])
          ("Curseforge" . webjump-to-curseforge))
      '';
      config = ''
        (defun webjump-to-curseforge (name)
          (let* ((prefix "legacy.curseforge.com/minecraft/")
                 (category (completing-read "Choose a category" '("mc-mods" "modpacks" "shaders" "data-pack" "texture-packs")))
        	 (url (concat prefix category))
        	 (term (webjump-read-string (concat name " Search for"))))
            (concat url "/search?search=" (webjump-url-encode term))))
      '';
    };

    elfeed = {
      enable = true;
      defer = true;
      custom.elfeed-feeds = '''("https://lukesmith.xyz/index.xml"
                                "https://youtube.com/feeds/videos.xml?channel_id=UCSJPFQdZwrOutnmSFYtbstA"
                                "https://planet.emacslife.com/atom.xml"
                                "https://youtube.com/feeds/videos.xml?channel_id=UC_GQ4mac4oN3wl1UdbFuTEA"
                                "https://youtube.com/feeds/videos.xml?channel_id=UC6UBbvEA8uh6Ulc6ax1Zs0g"
                                "https://youtube.com/feeds/videos.xml?channel_id=UCNzZD3otfZVlIdvYYRRqNSw"
                                "https://youtube.com/feeds/videos.xml?channel_id=UCnnkTXnyn0uZzmArZO99Klg"
                                "https://youtube.com/feeds/videos.xml?channel_id=UCq-VIBjS6Ia1r1IR_j-7NUw"
                                "https://youtube.com/feeds/videos.xml?channel_id=UC0E_vIe1e1lVeojYOgVg_5Q"
                                "https://youtube.com/feeds/videos.xml?channel_id=UCUQs6rEz6lRGHn6DWqss0hA"
                                "https://notrelated.xyz/rss")
        '';
      generalOne."efs/leader-keys"."r" = '''((lambda () (interactive) (elfeed) (elfeed-update)) :which-key "rss")'';
    };
    
  };

  programs.qutebrowser = {
    enable = true;
    enableDefaultBindings = true;
    package = (config.lib.nixGL.wrap pkgs-stable.qutebrowser);
  
    keyBindings = {
      normal = {
        "m" = "search-next";
        "M" = "search-prev";
        "e" = "fake-key <Down>";
        "o" = "fake-key <Up>";
        "O" = "scroll-page 0 -0.5";
        "E" = "scroll-page 0 0.5";
        "<ctrl-o>" = "scroll-page 0 -1";
        "<ctrl-e>" = "scroll-page 0 1";
        "t" = "mode-enter insert";
        "I" = "forward";
        "i" = "fake-key <Right>";
        "n" = "fake-key <Left>";
        "N" = "back";
        "k" = "hint links spawn mpv {hint-url}";
        "h" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher)'";
        "H" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher-tab)'";
        ";l" = "spawn --userscript qute-pass";
        ";u" = "spawn --userscript qute-pass --username-only";
        ";p" = "spawn --userscript qute-pass --password-only";
        ";o" = "spawn --userscript qute-pass --otp-only";
        ";P" = "spawn --userscript emacsclient-wrapper '(qutebrowser-pass \"{url}\")'";
        "a" = "cmd-set-text :";
        "<ctrl-f>" = "hint links spawn mpv {hint-url}";
      };
    };
  
    greasemonkey = [
      (pkgs.fetchurl {
    		url = "https://raw.githubusercontent.com/afreakk/greasemonkeyscripts/1d1be041a65c251692ee082eda64d2637edf6444/youtube_sponsorblock.js";
    		sha256 = "sha256-e3QgDPa3AOpPyzwvVjPQyEsSUC9goisjBUDMxLwg8ZE=";
    	})
      (pkgs.fetchurl {
    		url = "https://raw.githubusercontent.com/afreakk/greasemonkeyscripts/refs/heads/master/youtube_adblock.js";
    		sha256 = "sha256-AyD9VoLJbKPfqmDEwFIEBMl//EIV/FYnZ1+ona+VU9c=";
    	})
    ];
  
    aliases = {
            "q" = "quit";
            "w" = "session-save";
            "wq" = "quit --save";
          };
  
    loadAutoconfig = false;
    searchEngines = {
      "DEFAULT" = "https://search.inetol.net/search?q={}";
    };
    
    settings = {
      content = {
        blocking = {
          enabled = true;
          method = "both";
          adblock.lists = [
            "https://easylist.to/easylist/easylist.txt"
            "https://easylist.to/easylist/easyprivacy.txt"
            "https://easylist.to/easylist/fanboy-social.txt"
            "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
            "https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt"
            #"https://gitlab.com/curben/urlhaus-filter/-/raw/master/urlhaus-filter.txt"
            "https://pgl.yoyo.org/adservers/serverlist.php?showintro=0;hostformat=hosts"
            "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/NorwegianExperimentalList%20alternate%20versions/NordicFiltersABP-Inclusion.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt"
            "https://www.i-dont-care-about-cookies.eu/abp/"
            "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
            "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt"
            "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/quick-fixes.txt"
          ];
        };
        autoplay = false;
        cookies.store = false;
        geolocation = false;
        private_browsing = true;
      };
      completion = {
        cmd_history_max_items = 0;
        web_history.max_items = 0;
      };
  
      downloads.location.directory = "~/dwn";
      url = {
        default_page = "https://search.inetol.net";
        start_pages = "https://search.inetol.net";
      };
  
      window.transparent = true;
      colors = {
        completion = {
          category = {
            bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
            border = {
              bottom = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
              top = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
            };
          };
          even.bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
          odd.bg = lib.mkForce ("#90" + config.lib.stylix.colors.base00);
        };
        statusbar = {
          command = {
            bg = lib.mkForce ("#50" + config.lib.stylix.colors.base00);
            private.bg = lib.mkForce ("#50" + config.lib.stylix.colors.base00);
          };
        };
      };
  
      fileselect = {
        handler = "external";
        single_file.command = [
          "emacsclient"
          "{}"
        ];
        folder.command = [
          "emacsclient"
          "{}"
        ];
        multiple_files.command = [
          "emacsclient"
          "{}"
        ];
      };
  
      tabs = {
        tabs_are_windows = true;
        show = "never";
      };
      
      window.title_format = "{current_title}";
      new_instance_open_target = "tab-silent";
      statusbar.show = "never";
  
      hints.chars = "crstbfneia";
  
    };
  
    extraConfig = ''
      config.set('content.cookies.accept', 'no-3rdparty', 'chrome-devtools://*')
      config.set('content.cookies.accept', 'no-3rdparty', 'devtools://*')
      
      config.set('content.images', True, 'chrome-devtools://*')
      config.set('content.images', True, 'devtools://*')
      
      config.set('content.javascript.enabled', True, 'chrome-devtools://*')
      config.set('content.javascript.enabled', True, 'devtools://*')
      config.set('content.javascript.enabled', True, 'chrome://*/*')
      config.set('content.javascript.enabled', True, 'qute://*/*')
      
      config.set('content.notifications.enabled', False, 'https://www.reddit.com')
      config.set('content.notifications.enabled', False, 'https://www.youtube.com')
      
      config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
      config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://accounts.google.com/*')
      config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info} rv:110.0) Gecko/20100101 Firefox/119.0', 'https://*.slack.com/*')
      config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://docs.google.com/*')
      config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/119.0', 'https://drive.google.com/*')
      
      c.editor.command = ['emacsclient', '{file}']
      
      c.fonts.default_family = '"Source Code Pro"'
      c.fonts.default_size = '8pt'
      c.fonts.completion.entry = '8pt "Source Code Pro"'
      c.fonts.debug_console = '8pt "Source Code Pro"'
      c.fonts.prompts = 'default_size sans-serif'
    '';
  };
  
  programs.emacs.init.usePackage = {
    qutebrowser = {
      enable = true;
      ghook = [
        "('exwm-init-hook 'global-qutebrowser-exwm-mode)"
      ];
      gfhook = [
        "('server-visit-hook 'qute/dired-hook)"
        "('qutebrowser-exwm-mode-hook 'evil-normal-state)"
      ];
      config = ''
      (define-minor-mode qute-dired-mode
        "Used for dired buffers qutebrowser is using as a file picker"
        :keymap '())
      
      (general-def qute-dired-mode-map
              "C-c C-c" #'qute/choose-file)
      
      (general-def 'normal qute-dired-mode-map
        "i" 'dired-find-file
        "n" 'dired-up-directory)
      
      (defun qute/choose-file ()
        (interactive)
        (let ((files (dired-get-marked-files)))
          (with-temp-file qute-filename
            (insert (s-join "\n" files)))
          (remove-hook 'dired-mode-hook 'qute-dired-mode)
          (dolist (buffer dired-buffers) (when qute-dired-mode (kill-buffer (cdr buffer))))))
      
      (defun qute/dired-hook (&optional _)
        (when (s-starts-with? "/tmp/qutebrowser-fileselect" buffer-file-name)
          (setq qute-filename buffer-file-name)
          (kill-buffer)
          (add-hook 'dired-mode-hook 'qute-dired-mode)
          (setq qute-dired-buffers (list (dired "~/")))))
    '';
    };
    qutebrowser-evil = {
      enable = true;
      package = epkgs: epkgs.qutebrowser;
      ghook = [
        "('global-qutebrowser-exwm-mode-hook 'qutebrowser-evil-state-mode)"
      ];
    };
  };
}
