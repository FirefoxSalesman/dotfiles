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
	ezf
        hdmihelper
        pkg
        rustc
        cargo
        uv
        # python3
        cmake
        firefox
      ];
      programs.emacs.init.usePackage = {
        agent-shell = {
          enable = true;
          custom.agent-shell-cursor-acp-command = [
            ''"cursor-agent"''
            ''"acp"''
          ];
	  generalOne.global-leader = {
	    "a" = '''(:ignore t :which-key "agent-shell")'';
	    "af" = "'agent-shell-send-file";
	    "as" = "'agent-shell-cursor-start-agent";
	    "ar" = "'agent-shell-send-region";
	  };
        };

        popper.setopt.popper-reference-buffers = [ "'agent-shell-mode" ];
      };
    };
}
