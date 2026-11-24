{
  flake.homeModules.aesthetics.programs.emacs.init.usePackage.comet-trail = {
    enable = true;
    config = "(hook! 'fundamental-mode 'comet-trail-mode)";
    afterCall = [ "on-init-ui-hook" ];
  };
}
