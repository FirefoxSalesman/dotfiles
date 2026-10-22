{
  flake.homeModules.extraPackages =
    { config, pkgs, ... }:

    {
      home.packages = with pkgs; [
        gimp
        blockbench
        tor-browser
        wget
        zbar
        # scripts
        hdmihelper
        pkg
        rustc
        cargo
        uv
        python3
        cmake
        firefox
      ];
      programs.emacs.init.usePackage = {
        agent-shell = {
          enable = true;
          extraPackages = [ pkgs.cursor-cli ];
          custom.agent-shell-cursor-acp-command = [
            ''"cursor-agent"''
            ''"acp"''
          ];
        };

        popper.setopt.popper-reference-buffers = [ "'agent-shell-mode" ];
      };
    };
}
