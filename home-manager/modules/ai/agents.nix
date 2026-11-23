{
  flake.homeModules.ai.programs.emacs.init.usePackage = {
    agent-shell = {
      enable = true;
      custom.agent-shell-cursor-acp-command = [
        ''"cursor-agent"''
        ''"acp"''
      ];
      hook = [
        {
          namesDeferred = [ "agent-shell-mode" ];
          actions = [ "(lambda () (display-line-numbers-mode -1))" ];
        }
      ];
      generalOne.global-leader = {
        "a" = "'(:ignore t)";
        "as" = "'agent-shell";
        "ar" = "'agent-shell-send-region";
      };
    };

    popper.setopt.popper-reference-buffers = [ "'agent-shell-mode" ];
  };
}
