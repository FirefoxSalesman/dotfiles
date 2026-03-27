{
  flake.homeModules.exwm = { ... }:
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
	ghookf = ["('exwm-init 'tab-bar-mode)"];
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
               ("u" #'tab-undo "undo"))
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
    };
  };
}
