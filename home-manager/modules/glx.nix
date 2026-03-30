{ inputs, ... }:
{
  flake = {
    gpuWrappers = inputs.nixgl.defaultPackage;
    homeModules.glx = { config, lib, ... }: {
      targets.genericLinux.nixGL.packages = inputs.nixgl.packages;

      targets.genericLinux.enable = true;

      home.activation.clearNixglCache = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
	  [ -v DRY_RUN ] || rm -f ${config.xdg.cacheHome}/nixgl/result*
	'';
    };
  };
}
