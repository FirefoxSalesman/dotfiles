{ ... }:

{
  programs.emacs.init.usePackage = {
    helpful = {
      enable = true;
      defer = true;
      generalOne = {
        global-leader = {
          "hf" = "'helpful-function";
          "hs" = "'helpful-symbol";
          "hv" = "'helpful-variable";
          "hx" = "'helpful-command";
          "hk" = "'helpful-key";
          "hm" = "'helpful-macro";
        };
        embark-become-help-map = {
          "f" = "'helpful-function";
          "s" = "'helpful-symbol";
          "v" = "'helpful-variable";
        };
        embark-symbol-map."h" = "'helpful-symbol";
      };
    };
  };
}
