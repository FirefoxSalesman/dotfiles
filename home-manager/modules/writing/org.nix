{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      epkgs = pkgs.emacs.pkgs;
    in
    {
      packages.org = epkgs.callPackage (
        {
          elpaBuild,
          fetchurl,
          lib,
        }:
        elpaBuild {
          pname = "org";
          ename = "org";
          version = "9.8.6";
          src = fetchurl {
            url = "https://elpa.gnu.org/packages/org-9.8.6.tar";
            sha256 = "sha256-QyrhwAW55Y4vtgMbIjSQOkNr+8uTSmXdumi2qc8dTIE=";
          };
          packageRequires = [ ];
          meta = {
            homepage = "https://elpa.gnu.org/packages/org.html";
            license = lib.licenses.free;
          };
        }
      ) { };
    };

  flake.homeModules.writing =
    { config, pkgs, ... }:

    {
      home.packages = with pkgs; [ libreoffice-fresh ];

      programs.emacs.init = {
        writing.denote = {
          enable = true;
          sequence = true;
        };
        completions.tempel.templates.org-mode.exp = ''"#+export: " q'';
        ide.languages.org = {
          enable = true;
          aesthetics = {
            enable = true;
            headerFont = config.stylix.fonts.sansSerif.name;
          };
          captureTemplates.enable = true;
        };

        usePackage = {
          org = {
            gfhookf = [
              ''
                ('org-mode (list 'ispell-minor-mode
                	    (local! completion-at-point-functions (list (cape-capf-super
                                                                           #'tempel-complete
                						           #'cape-file)
                                                                        #'pcomplete-completions-at-point
                						        #'ob-gptel-capf
                						        #'cape-tex
                						        #'cape-elisp-block
                						        #'cape-dict
                                                                        #'cape-dabbrev))))''
            ];
            custom.org-directory = ''"~/doc"'';
            setopt = {
              org-export-with-section-numbers = false;
              org-export-with-toc = false;
            };
            generalTwoConfig.local-leader.org-mode-map = {
              "a" = '''("avy" . avy-org-goto-heading-timer)'';
              "e" = "'smart-export";
            };

            config = ''
              	    (require 'ol-man)
              	    (defun smart-export ()
              	      "Export the current buffer, according to its heading."
              	      (interactive)
              	      (let ((export-type
              	             (cadr (assoc "EXPORT" (org-collect-keywords '("EXPORT"))))))
              	        (cond
              	         ((equal export-type "pdf")
              	          (org-latex-export-to-pdf))
              	         ((equal export-type "odt")
              	          (org-odt-export-to-odt))
              	         ((equal export-type "md")
              	          (org-md-export-to-markdown))
              	         ((equal export-type "html")
              	          (org-html-export-to-html)))))
              	  '';
          };

          org-auto-tangle = {
            enable = true;
            ghookf = [ "('org-mode 'org-auto-tangle-mode)" ];
          };

          denote.setopt.denote-known-keywords = [
            ''"quotes"''
            ''"chem"''
            ''"emacs"''
            ''"java"''
            ''"physics"''
            ''"calculus"''
            ''"minecraft"''
            ''"de"''
            ''"proofs"''
            ''"csse230"''
            ''"os"''
            ''"databases"''
            ''"scifi"''
            ''"softwarerequirements"''
            ''"anthropology"''
            ''"theoryofcomputation"''
            ''"parallelcomp"''
            ''"cybersecurity"''
            ''"probstats"''
            ''"scheme"''
            ''"dreams"''
            ''"softwaredevelopment"''
            ''"ethics"''
            ''"plp"''
            ''"malwareanalysis"''
            ''"bio"''
            ''"ai"''
            ''"resolve"''
            ''"psychology"''
            ''"algorithmanalysis"''
            ''"music"''
            ''"people"''
          ];

          biblio = {
            enable = true;
            generalOne.global-leader = {
              "ob" = '''(:ignore t :which-key "biblio")'';
              "obl" = "'biblio-lookup";
              "obi" = "'biblio-doi-insert-bibtex";
            };
          };
        };
      };
    };
}
