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

  flake.homeModules.web = { ... }: {
    programs = {
      emacs.init.usePackage = {
        qutebrowser-exwm = {
          enable = true;
          package = epkgs: epkgs.qutebrowser;
          ghookf = [
            "('exwm-init 'global-qutebrowser-exwm-mode)"
          ];
          gfhookf = [
            "('qutebrowser-exwm-mode 'evil-normal-state)"
          ];
          setopt.qutebrowser-history-database = ''"~/.local/qutebrowser/history.sqlite"'';
	  # It gets grumpy if I don't redefine the function for some reason.
          config = ''
            (defun qutebrowser-exwm--update-window-info (window-info)
              "Update buffer-local variables from WINDOW-INFO."
              (when-let* ((x11-win-id (plist-get window-info :x11-win-id))
                          (buffer (exwm--id->buffer x11-win-id)))
                (with-current-buffer buffer
                  (qutebrowser--with-plist window-info
                    (win-id (setq-local qutebrowser-exwm-win-id win-id))
                    (mode (setq-local qutebrowser-exwm-keymode mode))
                    (icon-file (qutebrowser-exwm--update-favicon icon-file))
                    (search (setq-local qutebrowser-exwm-current-search search))
                    (hover (when (string= hover "") (setq hover nil))
                           (setq-local qutebrowser-exwm-hovered-url hover))
                    (url (setq-local qutebrowser-exwm-current-url
                                     (unless (string-empty-p url) url))
                         (setq-local buffer-file-name
                                     (when (string-prefix-p "file://" url)
                                       (string-replace "file://" "" url))))
                    (x-scroll-perc (setq-local qutebrowser-exwm-x-scroll-perc x-scroll-perc))
                    (y-scroll-perc (setq-local qutebrowser-exwm-y-scroll-perc y-scroll-perc))
                    (private (setq-local qutebrowser-exwm-private private))
                    (recently-audible (setq-local qutebrowser-exwm-recently-audible recently-audible))))))
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
  };
}
