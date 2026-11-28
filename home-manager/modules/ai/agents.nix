{
  flake.homeModules = {
    ai.programs.emacs.init.usePackage = {
      agent-shell = {
        enable = true;
        custom.agent-shell-cursor-acp-command = [
          ''"cursor-agent"''
          ''"acp"''
        ];
        hook = [
          {
            namesDeferred = [ "agent-shell-mode" ];
            actions = [
              "(lambda () (display-line-numbers-mode -1))"
              "(local! completion-at-point-functions (list 'agent-shell--command-completion-at-point 'agent-shell--file-completion-at-point 'cape-file 'cape-dict 'cape-dabbrev))"
            ];
          }
        ];
        generalOne.global-leader = {
          "a" = "'(:ignore t)";
          "as" = "'agent-shell";
          "ar" = "'agent-shell-send-region";
        };
        config = ''
          (with-eval-after-load 'evil-collection-agent-shell
              (efs/evil-collection-remap 'evil-collection-agent-shell-setup
               'insert agent-shell-mode-map
               "S-<return>" nil
               "S-RET" nil))
        '';
      };

      popper.setopt.popper-reference-buffers = [ "'agent-shell-mode" ];
    };

    personalAI.programs = {
      emacs.init.usePackage.agent-shell.setopt.agent-shell-opencode-default-model-id =
        ''"opencode/deepseek-v4.1-flash-free"'';
      opencode = {
        enable = true;
        enableMcpIntegration = true;
        context = "When you are in a JJ repository, always start a new change before doing any work, & describe that change after you finish your work.";
      };
    };
  };
}
