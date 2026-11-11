{
  flake.homeModules.development =
    { pkgs, ... }:

    {
      programs.emacs.init = {
        ide = {
          citre.enable = true;
          dape.enable = true;
          direnv = true;
          treesit-fold.enable = true;
          languages = {
            bash.enable = true;
            json.enable = true;
            lua.enable = true;
            toml.enable = true;
            yaml.enable = true;
            xml.enable = true;
            zenscript.enable = true;
          };
        };

        # Yoinked from heks-emacs
        completions.tempel.templates.lisp-mode.lambda = ''"(lambda (" p ")" n> r> ")"'';

        tools.apheleia = {
          enable = true;
          autoFormat = true;
        };

        usePackage = {
	  eat.init = ''
	    (defun efs/eat-command (command) (require 'eat) (eat command 'N))
	    (defun efs/eat-docker-command (container) (efs/eat-command (concat "docker exec -it '" container "' bash -c '. /opt/tetramem/python_env/bin/activate; bash -i'")))
	    (defun efs/enter-excelsior () (interactive) (efs/eat-docker-command "excelsior-sw-environ"))
	    (defun efs/enter-sdk () (interactive) (efs/eat-docker-command "tetramem-sdk"))
	    (defun efs/openocd () (interactive) (efs/eat-command "/mnt/nfs/ssunil/toolchains/hermetic/openocd/bin/openocd -c \"bindto 0.0.0.0\" -c \"set JTAGSN MLX200EVK025C\" -f board/mlx200_rv64_ft4232ha_evk.cfg"))
	    (defun efs/run-excelsior () (interactive) (shell-command "docker run -dit --name excelsior-sw-environ --replace --mount \"src=$HOME/.ssh,target=/root/.ssh,type=bind\" --mount \"src=$HOME/tetramem,target=/root/tetramem,type=bind\" --mount \"src=$HOME/kernelagent,target=/root/kernelagent,type=bind\" --mount \"src=/dev/ftdi-MLX200EVK025C-ch02,target=/dev/ftdi-MLX200EVK025C-ch02,type=bind\" --device \"/dev/ft4232ha_MLX200EVK025C_uart:/dev/ft4232ha_MLX200EVK025C_uart\" --network host 'excelsior-sw-environ'"))
	  '';
          rainbow-delimiters = {
            enable = true;
            ghookf = [ "('prog-mode 'rainbow-delimiters-mode)" ];
          };

          racket-mode.gfhookf = [ "('racket-mode 'hs-minor-mode)" ];

          toml-ts-mode = {
            extraPackages = [ pkgs.tombi ];
            eglot = ''("tombi" "lsp")'';
          };

          popper.setopt.popper-reference-buffers = [ "'dape-repl-mode" ];
        };
      };
    };
}
