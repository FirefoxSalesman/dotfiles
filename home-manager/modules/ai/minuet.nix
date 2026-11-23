{
  flake.homeModules.workAI.programs.emacs.init = {
    ai.minuet.enable = true;
    usePackage.minuet = {
      setopt.minuet-provider = "'openai-fim-compatible";
      hook =
        let
          mkModePrompt = mode: engineKind: responseKind: {
            namesDeferred = [ mode ];
            actions = [
              ''(efs/minuet-prompt-builder "You are an AI ${engineKind} completion engine. ${responseKind}\n")''
            ];
          };
        in
        [
          {
            names = [
              "eshell-mode"
              "text-mode"
            ];
            actions = [ "'minuet-auto-suggestion-mode" ];
          }
          {
            namesDeferred = [ "doom-escape" ];
            actions = [ "'minuet-dismiss-suggestion" ];
          }
          (mkModePrompt "eshell-mode" "command line"
            "Provide contextually appropriate completions: shell commands when the user is writing shell commands & emacs lisp when the user is writing emacs lisp."
          )
          (mkModePrompt "text-mode" "document" "Provide completions for the user's prose.")
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
