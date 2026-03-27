{
  flake.homeModules.exwm = { ... }:

  {
    programs.emacs.init.usePackage = {
      bufler = {
	enable = true;
	ghookf = ["('tab-bar-mode 'bufler-mode)"];
	general = {
	  "s-b" = "'bufler-hydra/body";
	  "s-f" = "'bufler-workspace-focus-buffer";
	  "s-F" = "'bufler-workspace-set";
	};
	generalTwo."'normal".evil-collection-unimpaired-mode-map = {
	  "]b" = "'bufler-cycle-buffers-forward";
	  "[b" = "'bufler-cycle-buffers-backward";
	};
	custom.bufler-groups = ''
	  (bufler-defgroups
	    ;; Subgroup collecting all named workspaces.
	    (group (auto-workspace))
	    ;; Subgroup collecting buffers in a project.
	    (group (auto-project))
	    ;; Subgroup collecting tramp buffers
	    (group (auto-tramp))
	    ;; Grouping browser windows
	    (group
	     (group-or "Browsers"
	               (name-match "Qutebrowser" (rx bos "Qutebrowser"))
	               (name-match "Tor Browser" (rx bos "Tor Browser"))
	               (mode-match "eww-mode" (rx bos "eww-"))))
	    (group
	     (group-or "Chat"
	               (name-match "Thunderbird" (rx bos "Thunderbird"))
	               (name-match "teams-for-linux" (rx bos "teams-for-linux"))
	               (name-match "zoom" (rx bos "zoom"))
	               (mode-match "ement" (rx bos "ement-"))
	               (name-match "vesktop" (rx bos "vesktop"))))
	    (group
	     (group-or "Media"
	               (name-match "mpv" (rx bos "Mpv"))
	               (mode-match "elfeed-search-mode" (rx bos "elfeed-"))
	               (mode-match "elfeed-show-mode" (rx bos "elfeed-"))
	  	     (mode-match "yeetube-mode" (rx bos "yeetube-"))))
	    (group
	     (group-or "Agenda"
	               (name-match "tasks.org" (rx bos "tasks.org"))
	               (mode-match "org-agenda-mode" (rx bos "org-agenda-"))))
	    (group
	     (group-or "Ledger"
	  	     (mode-match "ledger-mode" (rx bos "ledger-"))
	  	     (mode-match "ledger-report-mode" (rx bos "ledger-"))))
	    (group
	     (group-or "Notes"
	               (dir "~/doc/denote/")))
	    (group
	     (group-or "AV"
	               (name-match "lmms" (rx bos "lmms"))
	               (name-match "Gimp" (rx bos "Gimp"))
	               (name-match "Audacity" (rx bos "Audacity"))
	               (name-match "kdenlive" (rx bos "kdenlive"))))
	    (group
	     (group-or "Minecraft"
	               (dir "~/.local/PrismLauncher/")
	               (name-match "Minecraft" (rx bos "Minecraft"))
	               (name-match "PrismLauncher" (rx bos "PrismLauncher"))))
	    (group
	     (group-or "Games"
	               (dir "~/game")
	               (name-match "retroarch" (rx bos "retroarch"))
	               (name-match "PPSSPPSDL" (rx bos "PPSSPPSDL"))
	               (name-match "net.lutris.Lutris" (rx bos "net.lutris.Lutris"))))
	    (group
	     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
	     (group-or "Help/Info"
	               (mode-match "*Info*" (rx bos "info-"))
	  	     (mode-match "Man-mode" (rx bos "Man-"))))
	    (group
	     ;; Subgroup collecting all special buffers (i.e. ones that are not
	     ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
	     ;; through to other groups, so they end up grouped with their project buffers).
	     (group-and "*Special*"
	                (name-match "**Special**"
	                            (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
	                (lambda (buffer)
	                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
	                                       buffer)
	                              (funcall (mode-match "Dired" (rx bos "dired"))
	                                       buffer)
	                              (funcall (auto-file) buffer))
	                    "*Special*"))))
	    (auto-directory))
	'' ;
	init = ''
	  ;; These functions were adapted from perspective-exwm
	  (defun bufler-cycle-buffers (proc)
	    "Switches to the next or previous buffer in the workspace, if one exists, or the next buffer anywhere, if one doesn't exist."
	    (let* ((workspace (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find))))
	      (if workspace
	          (let* ((current (current-buffer))
	                 (buffer-list (mapcar #'cdr
	                                      (bufler-buffer-alist-at workspace :filter-fns bufler-filter-buffer-fns)))
	                 (current-pos (or (cl-position current buffer-list) -1))
	                 (len (length buffer-list))
	                 (next-pos (% (+ current-pos len
	                                 (if (eq proc 'evil-next-buffer) (- len 1) -1))
	                              len))
	                 (next-buffer (nth next-pos buffer-list)))
	            (switch-to-buffer next-buffer))
	        (funcall proc))))
	  (defun bufler-cycle-buffers-forward ()
	    "Cycles the buffers in the workspace forward."
	    (interactive)
	    (bufler-cycle-buffers 'evil-next-buffer))
	  (defun bufler-cycle-buffers-backward ()
	    "Cycles the buffers in the workspace backward."
	    (interactive)
	    (bufler-cycle-buffers 'evil-prev-buffer))
	'';
	extraConfig = ''
	  :pretty-hydra
          ((:color amaranth)
            ("Move"
             (("o" bufler-cycle-buffers-backward "prev")
              ("e" bufler-cycle-buffers-forward "next"))
             "Tricks"
             (("/" consult-buffer "search" :color blue :exit t)
              ("i" ibuffer "list (ibuffer)" :color blue :exit t)
              ("k" kill-current-buffer "delete"))
             "Quit"
             (("<escape>" nil "quit" :color blue :exit t)
              ("<return>" nil "quit" :color blue :exit t))))
	'';
      };

      bufler-workspace-tabs = {
	enable = true;
	ghookf = ["('bufler-mode 'bufler-workspace-workspaces-as-tabs-mode)"];
	gfhookf = ["('bufler-workspace-workspaces-as-tabs-mode '(global-tab-line-mode burly-tabs-mode))"];
      };
    };
  };
}
