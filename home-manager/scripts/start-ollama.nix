  { config, pkgs }:

  pkgs.writeShellScriptBin "start-ollama" ''
    if [[ "$(pidof ollama)" -gt 0 ]]; then
        echo "ollama already running"
    else
        ${(config.lib.nixGL.wrap pkgs.ollama)}/bin/ollama serve
    fi
  ''
