{
  flake.homeModules.emacs = { ... }:

  {
    programs.emacs.init.earlyInit = ''
      (setq-default bidi-display-reordering 'left-to-right
                    bidi-paragraph-direction 'left-to-right)
      
      (setq highlight-nonselected-windows nil
            redisplay-skip-fontification-on-input t
            bidi-inhibit-bpa t
            read-process-output-max (* 4 1024 1024)
            warning-minimum-level :error
            ;; Scratch is an org mode buffer
            initial-major-mode 'org-mode
            initial-scratch-message "")
    '';
  };
}
