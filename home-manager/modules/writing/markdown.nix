{
  flake.homeModules.writing = { pkgs, ... }: {
    programs.emacs.init = {
      # ide.languages.markdown.enable = true;
      # Yoinked from heks-emacs
      completions.tempel.templates.markdown-ts-mode = {
        gitcollapse = ''
          & "## " (p "Heading") n n "<details>" n n
          "<summary>" (p "Sub Heading")  "</summary>" n n
          (r "Insert Link or comments") n n "</details>"'';
        bolditalics = ''"***" p "***"'';
        srcblock = ''"```" p n p n "```"'';
        src = ''"'" p "'"'';
        unorderlist = ''& "- " (p "First") n> "- " (p "Second") n> "- " (p "Third")'';
        orderlist = ''& "1. " (p "First") n> "2. " (p "Second") n> "3. " (p "Third")'';
        insertlink = ''"[" p "](" p ")"'';
        hugotitle = ''& "+++" n "title = " (p "title") n "date = " (format-time-string "%Y-%m-%d") n "tags = [ " (p "tag1, tag2 ") "]" n "draft = false" n "+++"'';
        h1 = ''& "# " p " #"'';
        h2 = ''& "## " p " ##"'';
        h3 = ''& "### " p " ###"'';
        h4 = ''& "#### " p " ####"'';
        inserttable = "(call-interactively #'markdown-ts-table-insert-table)";
      };
      usePackage = {
        markdown-ts-mode = {
          enable = true;
          defer = true;
          extraPackages = [ pkgs.marksman ];
          mode = [ ''("\\.md\\'" . markdown-ts-mode)'' ];
          eglot = ''("marksman" "server")'';
          symex = true;
          gfhookf = [ "('markdown-ts-mode '(efs/markdown-font-setup) (lambda () (display-line-numbers-mode 0)))" ];
          init = ''
            (defun efs/markdown-font-setup ()
              (variable-pitch-mode)
              (dolist (face
                       '((markdown-ts-heading-1 . 1.4)
                         (markdown-ts-heading-2 . 1.2)
                         (markdown-ts-heading-3 . 1.1)
                         (markdown-ts-heading-4 . 1.05)
                         (markdown-ts-heading-5 . 1.05)
                         (markdown-ts-heading-6 . 1.05)))
                (set-face-attribute (car face) nil
                                    :font "SF Pro"
                                    :weight 'regular
                                    :height (cdr face))))
          '';
	  config = ''
            (evil-declare-motion 'markdown-ts-move-to-next-code-block)
            (evil-declare-motion 'markdown-ts-move-to-previous-code-block)
	  '';
	  generalTwo.":m".markdown-ts-mode-map = {
	    "]c" = "'markdown-ts-move-to-next-code-block";
	    "[c" = "'markdown-ts-move-to-previous-code-block";
	    "]h" = "'outline-next-heading";
	    "[h" = "'outline-previous-heading";
	    "M-o" = "'outline-move-subtree-up";
	    "M-e" = "'outline-move-subtree-down";
	    "M-n" = "'markdown-ts-promote";
	    "M-i" = "'markdown-ts-demote";
	  };
        };

        markdown-ts-appear = {
          enable = true;
          defer = true;
          ghookf = [ "('markdown-ts-mode 'markdown-ts-appear-mode)" ];
        };
      };
    };
  };
}
