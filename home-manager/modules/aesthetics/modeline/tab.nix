{
  flake.homeModules.aesthetics = { ... }:
  {
    programs.emacs.init.usePackage.tab-bar = {
      custom.tab-bar-format = ["'tab-bar-format-tabs-groups" "'tab-bar-separator" "'doom-nano-tabline" "'tab-bar-format-align-right" "'tab-bar-format-global"];
      setopt = {
	tab-bar-close-button-show = false;
	tab-bar-auto-width-max = ["'(150)" "20"];
      };
    };
  };
}
