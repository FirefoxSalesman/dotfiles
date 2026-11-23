{ self, ... }:
{
  flake.homeModules.personalAI =
    { pkgs, config, ... }:
    {
      programs.emacs.init.usePackage.gptel.config = ''
        (gptel-make-gh-copilot "copilot")
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda () (getenv "OPENROUTER_API_KEY"))
          :models '(nvidia/nemotron-3.5-super-vl:free))
      '';
    }
    // self.mkEnvVar pkgs config "openrouter_api_key" "OPENROUTER_API_KEY";
}
