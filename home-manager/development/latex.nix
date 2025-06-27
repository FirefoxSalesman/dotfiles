{ pkgs, config, lib, ... } :

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.latex = {
    enable = lib.mkEnableOption "enables LaTeX support"; 
    magicLatexBuffer = lib.mkEnableOption "use magic-latex-buffer to prettify";
    opinionatedChanges = lib.mkEnableOption "use some opinionated changes that I stole from Emacs from Scratch";
    cdlatex = lib.mkEnableOption "Enable cdlatex in latex and org modes";
  };

  config = lib.mkIf ide.languages.latex.enable {
    # Infinitely many thanks to David Wilson for this one
    programs.emacs.init.usePackage = {
      tex = {
        enable = ide.languages.latex.magicLatexBuffer;
        package = epkgs: epkgs.auctex;
        init = ''(setq-default TeX-master nil)'';
        custom = {
          reftex-label-alist = '''(("\\poemtitle" ?P "poem:" "\\ref{%s}" nil ("poem" "poemtitle")))'';
          reftex-format-cite-function = ''
            '(lambda (key fmt)
              (let ((cite (replace-regexp-in-string "%l" key fmt))
                   (if ( or (= ?~ (string-to-char fmt))
                      (member (preceding-char) '(?\ ?\t |/n ?~)))
                       cite
                 (concat "~" cite)))))
          '';
          TeX-auto-save = "t";
          TeX-parse-self = "t";
          reftex-plug-into-AUCTeX = "t";
        };
      };

      bibtex-mode = {
        enable = true;
        mode = [''"\\.bib\\'"''];
        lsp = ide.lsp;
        eglot = ide.eglot;
        symex = ide.symex;
      };

      latex = {
        enable = true;
        package = epkgs: epkgs.auctex;
        extraPackages = with pkgs; [texliveFull];
        mode = [''("\\.tex\\'" . LaTeX-mode)''];
        symex = ide.symex;
        lsp = ide.lsp;
        eglot = ide.eglot;
      };
      
      magic-latex-buffer = {
        enable = ide.languages.latex.magicLatexBuffer;
        hook = ["(LaTeX-mode . magic-latex-buffer)"];
      };

      cdlatex = {
        enable = ide.languages.latex.cdlatex;
        hook = [
          "('LaTeX-mode . turn-on-cdlatex)"
          "('org-mode . org-cdlatex-mode)"
        ];
      };
    };
  };
}
