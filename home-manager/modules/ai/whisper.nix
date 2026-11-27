{ inputs, ... }:

{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.whisper = (
        epkgs.callPackage epkgs.trivialBuild rec {
          pname = "whisper";
          version = "current";
          src = inputs.whisper;

          propagatedUserEnvPkgs = with epkgs; [ ];

          buildInputs = propagatedUserEnvPkgs;
        }
      );
    };

  flake.homeModules.personalAI = { pkgs, ... }: {
    programs.emacs.init.usePackage = {
      whisper = {
        enable = true;
        extraPackages = [ pkgs.cmake ];
        generalOne.global-leader = {
          "W" = '''(:ignore t :which-key "Whisper")'';
          "Wr" = "'whisper-run";
          "Wf" = "'whisper-file";
        };
        setopt.whisper-install-directory = ''(concat user-emacs-directory "/whisper")'';
      };

      phony = {
        enable = true;
        after = [ "whisper" ];
        generalOne.global-leader."Wj" = "'phony-mode";
        preface = ''
          (require 'phony-parser)
          (defun sacha-phony-sanitize-literal (s)
            "Lowercase and remove punctuation in S."
            (replace-regexp-in-string "[-,\\.!?:]" "" (downcase s)))
          
          (defun sacha-phony-evaluate (sentence)
            "Evaluate SENTENCE.
          Throw 'not-parsed if not parsed.
          Based on `phony-test--evaluate'."
            (let ((parses (phony-parser--parse-utterance sentence)))
              (unless parses
                (signal 'sacha-phony-not-parsed sentence))
              (phony--evaluate-ast (caar parses))))
          
          (defun sacha-whisper-phony (text)
            "Evaluate TEXT using phony.el definitions.
          Lower-case the first letter of TEXT to match command definitions.
          Useful in `speech-input-commands'."
            (sacha-phony-evaluate
             (concat
              (downcase (substring text 0 1))
              (if (> (length text) 1)
                  (substring text 1)
                "")))
            "")
          
          (defmacro efs/jarvis (name pattern &rest rest)
            "Define a phony command prefaced with `jarvis'."
            `(phony-defun
              ,name ,(mapcar (lambda (p) (concat "jarvis" p)) pattern) ,rest))
        '';
        config = ''
          (phony-experimental-set-parser-literal-normalizer #'sacha-phony-sanitize-literal)
	  (phony-mode)
        '';
      };
    };
  };
}
