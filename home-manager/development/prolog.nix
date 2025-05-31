{ pkgs, ... }:

{
  programs.emacs.init.usePackage.prolog-mode = {
    enable = true;
    mode = [''"\\.pl$"''];
    generalTwo."local-leader".prolog-mode-map."r" = '''(run-prolog :which-key "run")'';
  };
}
