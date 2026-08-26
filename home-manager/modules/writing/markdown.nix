{
  flake.homeModules.writing.programs.emacs.init = {
    ide.languages.markdown.enable = true;
    # Yoinked from heks-emacs
    completions.tempel.templates.markdown-mode = {
      gitcollapse = ''
        & "## " (p "Heading") n n "<details>" n n
        "<summary>" (p "Sub Heading")  "</summary>" n n
        (r "Insert Link or comments") n n "</details>"'';
      bolditalics = ''"***" p "***"'';
      srcblock = "(call-interactively #'markdown-insert-gfm-code-block)";
      src = ''"'" p "'"'';
      unorderlist = ''& "- " (p "First") n> "- " (p "Second") n> "- " (p "Third")'';
      orderlist = ''& "1. " (p "First") n> "2. " (p "Second") n> "3. " (p "Third")'';
      insertimage = "(call-interactively #'markdown-insert-image)";
      insertlink = "(call-interactively #'markdown-insert-link)";
      hugotitle = ''& "+++" n "title = " (p "title") n "date = " (format-time-string "%Y-%m-%d") n "tags = [ " (p "tag1, tag2 ") "]" n "draft = false" n "+++"'';
      h1 = ''& "# " p " #"'';
      h2 = ''& "## " p " ##"'';
      h3 = ''& "### " p " ###"'';
      h4 = ''& "#### " p " ####"'';
      inserttable = "(call-interactively #'markdown-insert-table)";
    };
    usePackage.markdown = {
      generalOneConfig.markdown-mode-map."C-c C-e" = "'markdown-do";
      gfhookf = [ "('markdown-mode 'efs/markdown-font-setup)" ];
      setopt = {
        markdown-command = ''"multimarkdown"'';
        markdown-hide-markup = true;
      };
      generalTwoConfig = {
        ":nm".gfm-mode-map = {
          "[h" = "'markdown-previous-visible-heading";
          "]h" = "'markdown-next-visible-heading";
        };
      };
      preface = ''
        (defun efs/markdown-font-setup ()
          (variable-pitch-mode)
          (dolist (face
                   '((markdown-header-face-1 . 1.4)
                     (markdown-header-face-2 . 1.2)
                     (markdown-header-face-3 . 1.1)
                     (markdown-header-face-4 . 1.05)
                     (markdown-header-face-5 . 1.05)
                     (markdown-header-face-6 . 1.05)))
            (set-face-attribute (car face) nil
                                :font "SF Pro"
                                :weight 'regular
                                :height (cdr face))))
      '';
    };
  };
}
