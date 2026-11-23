{
  flake.homeModules.personalAI.programs.emacs.init.ai.copilot = {
    enable = true;
    keepOutOf = [
      "json5-ts-mode"
      "json-ts-mode"
      "zenscript-mode"
    ];
  };
}
