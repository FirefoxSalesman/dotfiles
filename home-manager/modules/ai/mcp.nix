{
  flake.homeModules.ai = { pkgs, lib, ... }: {
    programs.mcp = {
      enable = true;
      servers = {
        nixos = {
          enable = true;
          command = lib.getExe pkgs.mcp-nixos;
        };
        memory = {
          enable = true;
          command = lib.getExe pkgs.mcp-server-memory;
        };
        lsp = {
          enable = true;
          command = "${pkgs.nodejs}/bin/npx";
          args = [
            "-y"
            "@theupsider/lsp-mcp@latest"
          ];
        };
      };
    };
  };
}
