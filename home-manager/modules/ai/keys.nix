{ self, ... }:
{
  flake.homeModules.personalAI =
    { pkgs, config, ... }:
    {
      programs.emacs.init.usePackage.gptel.config = ''
	(gptel-make-gh-copilot "copilot")
	(gptel-make-openai
	 "OpenRouter"
	 :host "openrouter.ai"
	 :endpoint "/api/v1/chat/completions"
	 :stream t
	 :key (lambda () (getenv "OPENROUTER_API_KEY"))
	 :models '(nvidia/nemotron-3.5-super-vl:free))
	(gptel-make-openai
	 "Groq"
	 :host "api.groq.com"
	 :endpoint "/openai/v1/chat/completions"
	 :stream t
	 :key (lambda () (getenv "GROQ_API_KEY"))
	 :models
	 '(openai/gpt-oss-20b openai/gpt-oss-120b qwen/qwen3.8-27b))
	(with-eval-after-load 'gptel-agent
	  (gptel-make-preset
	   'subagent
	   :model 'nvidia/nemotron-3.5-super-vl:free)
	  (setopt gptel-agent-preset 'subagent))
      '';
    }
    // self.mkEnvVar pkgs config "openrouter_api_key" "OPENROUTER_API_KEY"
    // self.mkEnvVar pkgs config "groq_api_key" "GROQ_API_KEY";
}
