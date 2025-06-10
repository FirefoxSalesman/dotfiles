{ pkgs, ... }:

{
  imports = [
    ./python.nix
  ];

  programs.emacs.init.usePackage.code-cells = {
    enable = true;
    demand = true;
    extraPackages = with pkgs; [python313Packages.jupytext];
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
