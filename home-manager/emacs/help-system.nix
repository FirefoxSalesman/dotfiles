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
      general = {
        "[remap describe-function]" = "'helpful-function";
        "[remap describe-symbol]" = "'helpful-symbol";
        "[remap describe-variable]" = "'helpful-variable";
        "[remap describe-command]" = "'helpful-command";
        "[remap describe-key]" = "'helpful-key";
      }; 
    };
  };
}
