{
  flake.homeModules.ai.programs.emacs.init = {
    ai.minuet.enable = true;
    usePackage.minuet = {
      setopt.minuet-provider = "'openai-fim-compatible";
      ghookf = [ "('(eshell-mode text-mode) 'minuet-auto-suggestion-mode)" ];
      gfhookf = [ "('doom-escape 'minuet-dismiss-suggestion)" ];
      config = ''
        (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
        (plist-put minuet-openai-fim-compatible-options :name "Ollama")
        (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
        (plist-put minuet-openai-fim-compatible-options :model "codegemma:2b")
        (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)
      '';
    };
  };
}
