{
  flake.homeModules.ai.programs.emacs.init = {
    ai.minuet.enable = true;
    usePackage =
      let
        mkMinuet = opt: ''
          (plist-put ${opt} :end-point "http://localhost:11434/v1/completions")
          (plist-put ${opt} :name "Ollama")
          (plist-put ${opt} :api-key "TERM")
          (plist-put ${opt} :model "qwen2.5-coder:1.5b")
	'';
      in
      {
        minuet = {
          setopt.minuet-provider = "'openai-fim-compatible";
          ghookf = [ "('(eshell-mode text-mode) 'minuet-auto-suggestion-mode)" ];
          gfhookf = [ "('doom-escape 'minuet-dismiss-suggestion)" ];
          config = ''
	    ${mkMinuet "minuet-openai-fim-compatible-options"}
            (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)
          '';
        };

        minuet-duet = {
          enable = false;
          setopt.minuet-duet-provider = "'openai-fim-compatible";
          ghookf = [ "('(eshell-mode prog-mode org-mode) 'minuet-duet-auto-mode)" ];
          gfhook = [ "('minuet-duet-auto-block-predicates 'efs/minuet-duet-only-in-evil-normal-p)" ];
          gfhookf = [ "('doom-escape 'minuet-duet-dismiss)" ];
          generalOneConfig = {
            ":n"."M-y" = "'minuet-duet-predict";
            minuet-duet-active-mode-map."C-i" = "'minuet-duet-apply";
          };
          preface = ''
            (defun efs/minuet-duet-only-in-evil-normal-p ()
              "Return non-nil to block duet prediction outside evil normal state."
              (not (and (bound-and-true-p evil-local-mode)
                        (evil-normal-state-p))))
          '';
          config = ''
	    ${mkMinuet "minuet-duet-openai-compatible-options"}
          '';
        };
      };
  };
}
