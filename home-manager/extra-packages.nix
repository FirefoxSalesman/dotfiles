{ config, pkgs, pkgs-stable, ... }:

{
  home.packages = with pkgs; [
    (config.lib.nixGL.wrap gimp)
    (config.lib.nixGL.wrap prismlauncher)
    (config.lib.nixGL.wrap ollama)
    zbar
    # comms
    (config.lib.nixGL.wrap teams-for-linux)
    (config.lib.nixGL.wrap thunderbird)
    (discord.override {
      withOpenASAR = true;
      withVencord = true;
    })
    # things emacs appreciates
    xclip
    xsel
    xdotool
    # scripts
    (import ./scripts/ezf.nix { inherit pkgs; })
    (import ./scripts/start-ollama.nix { inherit pkgs; })
    (import ./scripts/pkg.nix { inherit pkgs; })
  ];
}
