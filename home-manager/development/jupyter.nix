{ pkgs, ... }:

{
  imports = [
    ./python.nix
  ];

  home.packages = with pkgs; [
    python313Packages.jupytext
  ];

  programs.emacs.init.usePackage.code-cells = {
    enable = true;
    demand = true;
    generalTwo = {
      "'normal".code-cells-mode-map = {
        "M-e" = "'code-cells-forward-cell";
        "M-o" = "'code-cells-backward-cell";
      };
      "local-leader".code-cells-mode-map = {
        "e" = "'code-cells-eval";
      };
    };
  };
}
