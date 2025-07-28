  { ... }:

  {
    programs.emacs.init.usePackage = {
      which-key = {
        enable = true;
        defer = true;
        config = ''(which-key-enable-god-mode-support)'';
        ghook = ["('on-first-input-hook 'which-key-mode)"];
        custom = {
          which-key-idle-delay = "1";
          # which-key-popup-type = "'minibuffer";
        };
      };

      helpful = {
        enable = true;
        defer = true;
        generalOne."efs/leader-keys" = {
          "hf" = "'helpful-function";
          "hs" = "'helpful-symbol";
          "hv" = "'helpful-variable";
          "hx" = "'helpful-command";
          "hk" = "'helpful-key";
        };
      };
    };
  }
