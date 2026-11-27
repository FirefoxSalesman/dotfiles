{ self, ... }:
{
  flake.homeModules.aesthetics.programs.emacs.init.usePackage.writeroom-mode = {
    enable = true;
    hook = [
      {
        names = [
          "Man-mode"
          "org-agenda-mode"
          "org-mode"
          "Info-mode"
          "markdown-ts-mode"
          "shrface-mode"
        ];
        actions = [ "'writeroom-mode" ];
      }
      {
        namesDeferred = [ "writeroom-mode-enable" ];
        actions = [ "'visual-line-mode" ];
      }
    ];
    setopt = {
      writeroom-mode-line = true;
      writeroom-maximize-window = false;
      writeroom-global-effects = false;
    };
    generalOne.global-leader."w" = '''("writeroom" . writeroom-mode)'';
  };
}
