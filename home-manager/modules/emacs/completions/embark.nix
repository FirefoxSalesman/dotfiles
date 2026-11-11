{ inputs, ... }:

{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.embark = epkgs.callPackage (
        {
          org,
          consult,
          avy,
          compat,
          elpaBuild,
          fetchurl,
          lib,
        }:
        elpaBuild {
          pname = "embark";
          ename = "embark";
          version = "1.2";
          src = fetchurl {
            url = "https://elpa.gnu.org/packages/embark-1.2.tar";
            sha256 = "sha256-ZeRVPnCalhkBamNcQ19kit0xDHVgGzsIGMpxxbRmeOo=";
          };
          packageRequires = [
            org
            consult
            avy
            compat
          ];
          meta = {
            homepage = "https://elpa.gnu.org/packages/embark.html";
            license = lib.licenses.free;
          };
        }
      ) { };
    };

  flake.homeModules.emacs = { pkgs, ... }: {
    programs.emacs.init = {
      completions.smallExtras.embark = true;
      usePackage.embark = {
        general = {
          "M-a" = "'embark-dwim";
          "M-A" = "'embark-act";
        };
        generalTwo.":n".vertico-map."a" = "'embark-act";
        generalOneConfig = {
	  embark-region-map = {
	    "r" = '''(gptel-rewrite :which-key "rewrite with gptel")'';
	    "a" = '''(gptel-add :which-key "add to context")'';
	    "s" = '''(gptel-send :which-key "send to gptel")'';
	  };
	  embark-file-map = {
	    "a" = '''(gptel-add-file :which-key "add to context")'';
            "v" = "(my/embark-split-action find-file pertab-vertical-split)";
            "h" = "(my/embark-split-action find-file pertab-horizontal-split)";
            "t" = "(my/embark-split-action find-file tab-new)";
            "o" = "(my/embark-ace-action find-file)";
            "g" = "'efs/vcs-status";
          };
          embark-buffer-map = {
	    "a" = '''(gptel-add :which-key "add to context")'';
            "v" = "(my/embark-split-action switch-to-buffer pertab-vertical-split)";
            "h" = "(my/embark-split-action switch-to-buffer pertab-horizontal-split)";
            "t" = "(my/embark-split-action switch-to-buffer tab-new)";
            "o" = "(my/embark-ace-action switch-to-buffer)";
          };
          embark-bookmark-map = {
            "v" = "(my/embark-split-action bookmark-jump pertab-vertical-split)";
            "h" = "(my/embark-split-action bookmark-jump pertab-horizontal-split)";
            "t" = "(my/embark-split-action bookmark-jump tab-new)";
            "o" = "(my/embark-ace-action bookmark-jump)";
          };
          embark-symbol-map."R" = "'elisp-refs-symbol";
          embark-identifier-map."$" = "'jinx-correct";
	  embark-flymake-map."a" = '''(efs/embark-gptel-explain-diagnostic :which-key "explain diagnostic with gptel")'';
        };
        preface = ''
          (eval-when-compile
            (defmacro my/embark-split-action (fn split-type)
              `(defun ,(intern
                        (concat
                         "my/embark-" (symbol-name fn) "-"
                         (car
                          (last (split-string (symbol-name split-type) "-")))))
                   ()
                 (interactive)
                 (funcall #',split-type)
                 (call-interactively #',fn))))
          
          (eval-when-compile
            (defmacro my/embark-ace-action (fn)
              `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
                 (interactive)
                 (with-demoted-errors "%s"
                   (require 'ace-window)
                   (let ((aw-dispatch-always t))
                     (aw-switch-to-window (aw-select nil))
                     (call-interactively (symbol-function ',fn)))))))
          
          (defun efs/embark-gptel-explain-diagnostic ()
            "Ask gptel to explain the Flymake diagnostic at point."
            (interactive)
            (require 'gptel-quick)
            (if-let* ((diag (car (flymake-diagnostics (point)))))
                (let* ((text (format
                              "Explain this diagnostic in %s and suggest a fix.\n\n\
          Diagnostic (%s): %s\n\nCode:\n%s"
                              (buffer-name)
                              (flymake-diagnostic-type diag)
                              (flymake-diagnostic-text diag)
                              (buffer-substring-no-properties
                               (flymake-diagnostic-beg diag)
                               (flymake-diagnostic-end diag))))
                       )
                  (gptel-quick text))
              (user-error "No Flymake diagnostic at point")))
        '';
      };
    };
  };
}
