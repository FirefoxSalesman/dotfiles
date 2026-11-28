{ inputs, ... }:

{
  flake.homeModules = {
    emacs-host = {
      targets.genericLinux.gpu.nvidia = {
        enable = true;
        sha256 = "sha256-CqsFGNtTz3PEEtx/MrvIUMqV6DnYkj0N5L/0LByRac4=";
        version = "580.196.07";
      };
      imports = with inputs.self.homeModules; [
        personalMachine
        singleMonitorMachine
      ];
    };
    work-host = { pkgs, ... }: {
      imports = with inputs.self.homeModules; [
        singleMonitorMachine
        workAI
      ];
      home.packages = with pkgs; [
        rustc
        cargo
        uv
        cmake
        firefox
      ];
      programs.emacs.init = {
        ide.languages.python = {
          enable = true;
          wantRuff = true;
        };
        usePackage.ghostel.init = ''
          (defun efs/ghostel-command (command name) (require 'ghostel) (ghostel-shell-command command name))
          (defun efs/ghostel-docker-command (container) (efs/ghostel-command (concat "docker exec -it '" container "' bash -c '. /opt/tetramem/python_env/bin/activate; bash -i'") container))
          (defun efs/enter-excelsior () (interactive) (efs/ghostel-docker-command "excelsior-sw-environ"))
          (defun efs/enter-sdk () (interactive) (efs/ghostel-docker-command "tetramem-sdk"))
          (defun efs/openocd () (interactive) (efs/ghostel-command "/mnt/nfs/ssunil/toolchains/hermetic/openocd/bin/openocd -c \"bindto 0.0.0.0\" -c \"set JTAGSN MLX200EVK025C\" -f board/mlx200_rv64_ft4232ha_evk.cfg" "openocd"))
          (defun efs/run-excelsior () (interactive) (shell-command "docker run -dit --name excelsior-sw-environ --replace --mount \"src=$HOME/.ssh,target=/root/.ssh,type=bind\" --mount \"src=$HOME/tetramem,target=/root/tetramem,type=bind\" --mount \"src=$HOME/kernelagent,target=/root/kernelagent,type=bind\" --mount \"src=/dev/ftdi-MLX200EVK025C-ch02,target=/dev/ftdi-MLX200EVK025C-ch02,type=bind\" --device \"/dev/ft4232ha_MLX200EVK025C_uart:/dev/ft4232ha_MLX200EVK025C_uart\" --network host 'excelsior-sw-environ'"))
        '';
      };
    };
    monitors-host = {
      imports = [ inputs.self.homeModules.personalMachine ];
      hosts = {
        xrandr-command = ''
          xrandr --output DP-4 --primary --mode 1920x1080 --output DVI-I-1 --mode 1920x1200
        '';
        exwm-monitors = [
          ''0 "DP-4"''
          ''1 "DVI-I-1"''
        ];
        wantBattery = false;
      };
    };
    singleMonitorMachine = {
      hosts = {
        xrandr-command = ''
          xrandr --setprovideroutputsource modesetting NVIDIA-0
          xrandr --auto
          xrandr --dpi 96
        '';
        exwm-monitors = [ ''0 "eDP-1-1"'' ];
        wantBattery = true;
      };
    };
    personalMachine.imports = with inputs.self; [
      homeModules.gaming
      homeModules.ledger
      homeModules.webdev
      homeModules.personalAI
      homeModules.personalChat
    ];
  };
}
