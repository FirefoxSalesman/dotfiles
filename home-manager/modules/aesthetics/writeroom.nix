{
  flake.homeModules.aesthetics = { ... }:
  {
    programs.emacs.init.usePackage.writeroom-mode = {
      enable = true;
      ghookf = ["((gen-mode-hooks '(Man org-agenda org Info markdown shrface)) 'writeroom-mode)"];
      gfhookf = ["('writeroom-mode-enable 'visual-line-mode)"];
      setopt = {
	writeroom-mode-line = true;
	writeroom-maximize-window = false;
	writeroom-global-effects = false;
      };
      generalOne.global-leader."w" = '''("writeroom" . writeroom-mode)'';
    };
  };
}
