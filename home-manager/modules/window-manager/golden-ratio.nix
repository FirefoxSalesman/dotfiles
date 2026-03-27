{
  flake.homeModules.exwm = { pkgs, config, lib, ... }:

  {
    programs.emacs.init.usePackage = {
      golden-ratio = {
	enable = true;
	defer = true;
	ghookf = ["('on-first-input 'golden-ratio-mode)"];
	config = "(general-add-advice 'golden-ratio :after 'exwm-mff-warp-to-selected)";
      };

      exwm-mff = {
	command = ["exwm-mff-warp-to-selected"];
	hook = lib.mkForce [];
      };
    };
  } ;
}
