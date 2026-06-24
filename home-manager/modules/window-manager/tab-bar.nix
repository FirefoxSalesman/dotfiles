{ inputs, ... }:

{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
      roll = (
        epkgs.callPackage epkgs.trivialBuild rec {
          pname = "roll";
          version = "current";
          src = inputs.roll;
        }
      );
    in
    {
      packages = {
        roll = roll;
        pertab = (
          epkgs.callPackage epkgs.trivialBuild rec {
            pname = "pertab";
            version = "current";
            src = inputs.pertab;

            propagatedUserEnvPkgs = with epkgs; [
              elwm
              roll
            ];

            buildInputs = propagatedUserEnvPkgs;
          }
        );
      };
    };

  flake.homeModules.exwm =
    { ... }:
    let
      pertabExtension = {
        enable = true;
        package = epkgs: epkgs.pertab;
        after = [ "pertab" ];
      };
    in
    {
      programs.emacs.init.usePackage = {
        tab-bar = {
          enable = true;
          config = ''
            (general-add-advice 'tab-new :after #'dashboard-open)
            (defun efs/tab-bar-select ()
              (interactive)
              (setq tab-bar-tab-hints t)
              (tab-bar-select-tab (string-to-number (read-string "Tab Number: ")))
              (setq tab-bar-tab-hints nil))
          '';
          ghookf = [ "('exwm-init 'tab-bar-mode)" ];
          general."s-u" = "'tab-bar-hydra/body";
          setopt.tab-bar-select-restore-windows = false;
          extraConfig = ''
            	  :pretty-hydra
                  ((:color amaranth)
                   ("Navigation"
                    (("e" #'evil-tab-next "next")
                     ("o" #'tab-bar-switch-to-prev-tab "prev")
                     ("v" #'tab-recent "recent")
                     ("b" #'tab-bar-lost-commands-switch-to-first-tab "first")
                     ("B" #'tab-bar-lost-commands-switch-to-last-tab "last")
                     ("/" #'efs/tab-bar-select "search"))
                    "Creation/Deletion"
                    (("s" #'tab-new "new")
                     ("k" #'tab-close "close")
                     ("r" #'tab-rename "rename")
                     ("j" #'tab-undo "undo"))
                    "Groups"
                    (("g" #'tab-group "add to group")
                     ("K" #'tab-close-group "close group"))
                    "Organization"
                    (("E" #'tab-bar-lost-commands-move-tab-forward "forward")
                     ("O" #'tab-bar-lost-commands-move-tab-backward "backward"))
                    "Exit"
                    (("<return>" nil "" :color blue)
                     ("<escape>" nil "" :color blue))))
            	'';
        };

        tab-bar-lost-commands = {
          enable = true;
          command = [
            "tab-bar-lost-commands-move-tab-forward"
            "tab-bar-lost-commands-move-tab-backward"
            "tab-bar-lost-commands-switch-to-first-tab"
            "tab-bar-lost-commands-switch-to-last-tab"
          ];
        };

        pertab = {
          enable = true;
          ghookf = [ "('tab-bar-mode 'pertab-mode)" ];
          setopt = {
            pertab-default-layout = "'master-stack";
            pertab-next-buffer-function = "'bufler-cycle-buffers-forward";
            pertab-previous-buffer-function = "'bufler-cycle-buffers-backward";
            roll-debug-enabled = false;
          };
          gfhookf = [
            "('(pertab-follow-enter pertab-scroll-enter) (lambda () (golden-ratio-mode -1)))"
            "('(pertab-follow-exit pertab-scroll-exit) (lambda () (golden-ratio-mode +1)))"
          ];
        };

        pertab-monocle = pertabExtension;
        pertab-follow = pertabExtension;
        pertab-master-stack = pertabExtension;
        pertab-scroll = pertabExtension;
      };
    };
}
