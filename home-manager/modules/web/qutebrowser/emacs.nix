{ inputs, ... }:

{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.qutebrowser-emacs = (
        epkgs.callPackage epkgs.trivialBuild rec {
          pname = "qutebrowser";
          version = "current";
          src = inputs.exwm-qutebrowser;

          propagatedUserEnvPkgs = with epkgs; [
            consult
            exwm
            password-store
            dash
            evil
            password-store-otp
            doom-modeline
          ];

          buildInputs = propagatedUserEnvPkgs;
        }
      );
    };

  flake.homeModules.web.programs = {
    emacs.init.usePackage = {
      qutebrowser = {
        enable = true;
        setopt.qutebrowser-history-database = ''"~/.local/qutebrowser/history.sqlite"'';
      };

      qutebrowser-exwm = {
        enable = true;
        package = epkgs: epkgs.qutebrowser;
        ghookf = [
          "('exwm-init 'global-qutebrowser-exwm-mode)"
        ];
        gfhookf = [
          "('qutebrowser-exwm-mode 'evil-normal-state)"
        ];
        # setopt.qutebrowser-history-database = ''"~/.local/qutebrowser/history.sqlite"'';
        # It gets grumpy if I don't redefine the function for some reason.
        config = ''
          (defun qutebrowser-exwm--update-window-info (window-info)
            "Update buffer-local variables from WINDOW-INFO."
            (when-let* ((x11-win-id (plist-get window-info :x11-win-id))
                        (buffer (exwm--id->buffer x11-win-id)))
              (with-current-buffer buffer
                (qutebrowser--with-plist
                 window-info
                 (win-id (setq-local qutebrowser-exwm-win-id win-id))
                 (mode (setq-local qutebrowser-exwm-keymode mode))
                 (icon-file (qutebrowser-exwm--update-favicon icon-file))
                 (search (setq-local qutebrowser-exwm-current-search search))
                 (hover
                  (when (string= hover "")
                    (setq hover nil))
                  (setq-local qutebrowser-exwm-hovered-url hover))
                 (url
                  (setq-local qutebrowser-exwm-current-url
                              (unless (string-empty-p url)
                                url))
                  (setq-local buffer-file-name
                              (when (string-prefix-p "file://" url)
                                (string-replace "file://" "" url))))
                 (x-scroll-perc
                  (setq-local qutebrowser-exwm-x-scroll-perc x-scroll-perc))
                 (y-scroll-perc
                  (setq-local qutebrowser-exwm-y-scroll-perc y-scroll-perc))
                 (private (setq-local qutebrowser-exwm-private private))
                 (recently-audible
                  (setq-local qutebrowser-exwm-recently-audible
                              recently-audible))))))
          
          (with-eval-after-load 'gptel
            (gptel-make-tool
             :name "qutebrowser-open"
             :category "qutebrowser"
             :function 'qutebrowser-open-url
             :description "Open a new tab in qutebrowser."
             :args
             '((:name
                "url"
                :type "string"
                :description "The url to open or term to search for.")))
          
            (gptel-make-tool
             :name "qutebrowser-list"
             :category "qutebrowser"
             :function
             (lambda ()
               (mapcar
                (lambda (browser)
                  (qutebrowser-exwm-buffer-url browser))
                (qutebrowser-exwm-buffer-list)))
             :description "List all open qutebrowser urls."
             :args '())
          
            (gptel-make-tool
             :name "qutebrowser-read"
             :category "qutebrowser"
             :function
             (lambda ()
               (with-temp-buffer
                 (insert-file-contents
                  (let ((file (make-temp-file "")))
                    (qutebrowser-cmd-debug-dump-page file :plain)
                    (sleep-for .1)
                    file))
                 (buffer-string)))
             :description "Read the most recently opened tab's HTML."
             :args '())
          
            (gptel-make-tool
             :name "qutebrowser-surface"
             :category "qutebrowser"
             :function
             (lambda (url)
               (switch-to-buffer (qutebrowser-exwm-find-buffer url)))
             :description "Show a qutebrowser tab to the user."
             :args
             '((:name
                "url"
                :type "string"
                :description "The url of the page to show the user")))
          
            (gptel-make-preset
                'web-agent
              :description "An agent for qutebrowser"
              :system "You are an AI assistant that helps users accomplish their goals. Keep responses concise to the point of being terse
          - Avoid flattery, superlatives, or unnecessary flourishes
          - Prioritize accuracy over agreement
          - Challenge the user constructively when you can think of a better approach
          
          You have control over the user's web browser (qutebrowser) & should utilize it to assist the user.
          
          # Finding Information
          The simplest way to find information is to use the WebSearch tool. It will give you a handful of search results without much hassle. If you need to know more about any of the sites you find, WebFetch will get their contents.
          When reading links, you should prefer WebFetch.
          
          Should you find yourself in a scenario where you need access to javascript in order to read a website, you will need to use qutebrowser.
          1. Use qutebrowser-open to open the link.
          2. Use qutebrowser-read to get its contents.
          
          # Displaying Information
          If you are ever asked to find a site, you should use qutebrowser-open on that site's link.
          
          # Rules:
          - Do your best to answer the user's queries.
          - If a tool call fails, don't stop. Try again."
              :tools '("qutebrowser" "WebSearch" "WebFetch")))
        '';
      };

      qutebrowser-evil = {
        enable = true;
        package = epkgs: epkgs.qutebrowser;
        ghookf = [
          "('global-qutebrowser-exwm-mode 'qutebrowser-evil-state-mode)"
        ];
      };

      qutebrowser-fileselect = {
        enable = true;
        package = epkgs: epkgs.qutebrowser;
        ghookf = [
          "('global-qutebrowser-exwm-mode 'qutebrowser-fileselect-mode)"
        ];
        setopt.qutebrowser-fileselect-handler = "'qutebrowser-fileselect-completion";
      };

      popper.setopt.popper-reference-buffers = [''"^\\*qutebrowser-llm-response\\*"''];
    };

    qutebrowser.settings = {
      tabs = {
        tabs_are_windows = true;
        show = "never";
      };
      window.title_format = "{current_title}";
      new_instance_open_target = "tab-silent";
      statusbar.show = "never";
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
    };
  };
}
