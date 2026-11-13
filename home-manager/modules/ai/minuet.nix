{
  flake.homeModules.ai.programs.emacs.init = {
    ai.minuet.enable = true;
    usePackage.minuet = {
      setopt.minuet-provider = "'openai-fim-compatible";
      ghookf = [ "('(eshell-mode text-mode) 'minuet-auto-suggestion-mode)" ];
      gfhookf = [
        "('doom-escape 'minuet-dismiss-suggestion)"
        ''('eshell-mode (efs/minuet-prompt-builder "You are an AI command line completion engine. Provide contextually appropriate completions: shell commands when the user is writing shell commands & emacs lisp when the user is writing emacs lisp.\n"))''
        ''('text-mode (efs/minuet-prompt-builder "You are an AI document completion engine. Provide completions for the user's prose."))''
      ];
      preface = ''
        (defmacro efs/minuet-prompt-builder (text)
          "Constructs a minuet prompt that can be used in a hook. TEXT is the prompt's prefix."
          `(local!
            minuet-default-prompt
            (concat
             ,text
             "
        Input markers:
        - `<contextAfterCursor>`: Context after cursor
        - `<cursorPosition>`: Current cursor location
        - `<contextBeforeCursor>`: Context before cursor
        
        Note that the user input will be provided in **reverse** order: first the
        context after cursor, followed by the context before cursor.
        ")))
      '';
      config = ''
        (plist-put
         minuet-openai-fim-compatible-options
         :end-point "http://localhost:11434/v1/completions")
        (plist-put minuet-openai-fim-compatible-options :name "Ollama")
        (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
        (plist-put minuet-openai-fim-compatible-options :model "codegemma:2b")
        (minuet-set-optional-options
         minuet-openai-fim-compatible-options
         :max_tokens 56)
      '';
    };
  };
}
