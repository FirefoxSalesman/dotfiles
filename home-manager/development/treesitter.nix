{ pkgs, ... }:

{
  programs.emacs.init.usePackage = {
    treesit-auto = {
      enable = true;
      custom.treesit-auto-install = "'prompt";
      init = "(mp-setup-install-grammars)";
      config = "(global-treesit-auto-mode)";
      # stolen from mickey petersen
      extraConfig = ''
        :preface (defun mp-setup-install-grammars ()
                "Install Tree-sitter grammars if they are absent."
                (interactive)
                (dolist (grammar
                         '(;(xml "https://github.com/ObserverOfTime/tree-sitter-xml")
                           (toml "https://github.com/ikatyang/tree-sitter-toml")
                           (json5 "https://github.com/Joakker/tree-sitter-json5")
                           (elisp "https://github.com/Wilfred/tree-sitter-elisp")))
                  (add-to-list 'treesit-language-source-alist grammar)
                  ;; Only install `grammar' if we don't already have it
                  ;; installed. However, if you want to *update* a grammar then
                  ;; this obviously prevents that from happening.
                  (unless (treesit-language-available-p (car grammar))
                    (treesit-install-language-grammar (car grammar)))))
  '';
    };

    tree-sitter = {
      enable = true;
      afterCall = ["on-first-file-hook"];
      config = ''
        (global-tree-sitter-mode)
        (dolist (mode '((java-ts-mode . java)
          (html-ts-mode . html)
		      (lua-ts-mode . lua)
		      (go-ts-mode . go)
		      (python-ts-mode . python)
		      (scala-ts-mode . scala)
		      (js-ts-mode . javascript)
		      (json-ts-mode . json)
		      (gfm-mode . markdown)
		      (ruby-ts-mode . ruby)
		      (csharp-ts-mode . c-sharp)
		      (rust-ts-mode . rust)
		      (css-ts-mode . css)
		      (c-ts-mode . c)
		      (racket-repl-mode . racket)
		      (ess-r-mode . r)
		      (inferior-ess-r-mode . r)
		      (erlang-ts-mode . erlang)
		      (toml-ts-mode . toml)))
        (add-to-list 'tree-sitter-major-mode-language-alist mode))
      '';
    };

    tree-sitter-langs = {
      enable = true;
      custom.tree-sitter-langs-grammar-dir = ''"~/.cache/emacs/tree-sitter"'';
      afterCall = ["global-tree-sitter-mode-hook"];
    };
  };
}
