{ inputs, config, pkgs, pkgs-stable, lib, ... }:

{
  home.packages = with pkgs; [
    pkgs-stable.python39Packages.adblock
    (config.lib.nixGL.wrap tor-browser)
  ];

  programs.emacs.init.usePackage = {
    browse-url = {
      enable = true;
      custom = {
        browse-url-handlers = ''
          '(("https:\\/\\/www\\.youtu\\.*be." . efs/mpv-browse-url)
            ("https:\\/\\/yewtu\\.*be." . efs/mpv-browse-url)
            ("https:\\/\\/inv\\.*nadeko\\.*net\\/watch." . efs/mpv-browse-url)
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
          ("Curseforge" . [simple-query "legacy.curseforge.com" "legacy.curseforge.com/minecraft/mc-mods/search?search=" ""]))
      '';
    };

    elfeed = {
      enable = true;
      defer = true;
      custom.elfeed-feeds = '''("https://lukesmith.xyz/index.xml"
                                "https://yewtu.be/feeds/videos.xml?channel_id=UCSJPFQdZwrOutnmSFYtbstA"
                                "https://karthinks.com/index.xml"
                                "https://planet.emacslife.com/atom.xml"
                                "https://inv.nadeko.net/feeds/videos.xml?channel_id=UC_GQ4mac4oN3wl1UdbFuTEA"
                                "https://inv.nadeko.net/feeds/videos.xml?channel_id=UC6UBbvEA8uh6Ulc6ax1Zs0g"
                                "https://inv.nadeko.net/feeds/videos.xml?channel_id=UCNzZD3otfZVlIdvYYRRqNSw"
          		                  "https://inv.nadeko.net/feeds/videos.xml?channel_id=UC9OZkS1Mhl5UvKSiPrYqsxg"
          		                  "https://inv.nadeko.net/feeds/videos.xml?channel_id=UCgVLFBokgO85hyVl7tIoJvw"
                                "https://notrelated.xyz/rss")
        '';
      generalOne."efs/leader-keys"."r" = '''((lambda () (interactive) (elfeed) (elfeed-update)) :which-key "rss")'';
    };
    
  };

  programs.qutebrowser = {
    enable = true;
    enableDefaultBindings = true;
    package = (config.lib.nixGL.wrap pkgs.qutebrowser);
  
    keyBindings = {
      normal = {
        "search-next" = "m";
        "m" = "search-next";
        "M" = "search-prev";
        "e" = "fake-key <Down>";
        "o" = "fake-key <Up>";
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
      };
    };
  
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
            bg = lib.mkForce "#90001f28";
            border = {
              bottom = lib.mkForce "#90001f28";
              top = lib.mkForce "#90001f28";
            };
          };
          even.bg = lib.mkForce "#90001f28";
          odd.bg = lib.mkForce "#90001f28";
        };
        statusbar = {
          caret = {
            bg = lib.mkForce "#50009bda";
          };
      
          insert = {
            bg = lib.mkForce "#503f3f81";
          };
      
          private = {
            bg = lib.mkForce "#50001f28";
          };
      
          passthrough = {
            bg = lib.mkForce "#500096f1";
          };
      
          command = {
            bg = lib.mkForce "#50001f28";
      
            private = {
              bg = lib.mkForce "#50001f28";
            };
          };
          # progress.bg = info;
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
      
      window.title_format = "{current_title}{title_sep}{current_url}";
      new_instance_open_target = "tab-silent";
      statusbar.show = "never";
  
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
      c.fonts.statusbar = '8pt "Source Code Pro"'
      config.source("emacs_hooks.py")
      config.source("emacs_ipc.py")
    '';
  };
  
  programs.emacs.init.usePackage.qutebrowser = {
    enable = true;
    package = epkgs: (epkgs.callPackage ./emacs/emacs-packages/qutebrowser.nix {
      inherit inputs;
      inherit (epkgs) trivialBuild dash consult exwm password-store evil;
    });
    hook = [
      "(exwm-init . global-qutebrowser-exwm-mode)"
      "(server-visit . qute/dired-hook)"
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
}
