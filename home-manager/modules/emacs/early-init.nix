{
  flake.homeModules.emacs = { ... }:

  {
    programs.emacs.init.earlyInit = ''
      (scroll-bar-mode -1) ; Disable visible scrollbar
      (tool-bar-mode -1) ; Disable the toolbar
      (menu-bar-mode -1)
      
      (setq-default bidi-display-reordering 'left-to-right
                    bidi-paragraph-direction 'left-to-right
      	      cursor-in-non-selected-windows nil)
      
      (setq auto-save-visited-file-name t
            highlight-nonselected-windows nil
            redisplay-skip-fontification-on-input t
            bidi-inhibit-bpa t
            read-process-output-max (* 4 1024 1024)
            warning-minimum-level :error
            use-package-enable-imenu-support t
            make-backup-files nil
            enable-recursive-minibuffers t
            inhibit-startup-message t
            inhibit-startup-screen t
            visible-bell t
            use-short-answers t
            switch-to-buffer-obey-display-actions t
            ;; Scratch is an org mode buffer
            initial-major-mode 'org-mode
            initial-scratch-message ""
            ;;Reduce garbage
            user-emacs-directory "~/.cache/emacs")
      
      (defun efs/display-startup-time ()
        (message "Emacs loaded in %s with %d garbage collections."
                 (format "%.2f seconds"
                         (float-time
                          (time-subtract after-init-time before-init-time)))
                 gcs-done))
      
      (add-hook 'emacs-startup-hook #'efs/display-startup-time)
    '';
  };
}
