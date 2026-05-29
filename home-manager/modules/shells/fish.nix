{
  flake.homeModules.shellConfig = { lib, pkgs, ... }:
  {
    programs.fish = {
      enable = true;
      package = pkgs.fish;
      interactiveShellInit = ''${lib.getExe pkgs.pfetch}'';
      # Borrows from yay's completions
      completions.pkg = ''
	set -l listall "(yay -Pc)"
	set -l listinstalled "(pacman -Q | string replace ' ' \t)"
	complete -c pkg -a 'optimize update add rm flake tmp rebuild template query search help'
	complete -c pkg -n "__fish_seen_subcommand_from add" -xa "$listall"
	complete -c pkg -n "__fish_seen_subcommand_from rm" -xa "$listinstalled"
      '';
    };
  };
}
