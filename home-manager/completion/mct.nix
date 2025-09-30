{ config, lib, ... }:

let
  completions = config.programs.emacs.init.completions;
in
{
  options.programs.emacs.init.completions.mct.enable = lib.mkEnableOption "Enables mct. Largely stolen from Prot.";

  config.programs.emacs.init = lib.mkIf completions.mct.enable {
    hasOn = true;
    completions.smallExtras = {
      enable = true;
      embark = true;
    };
    usePackage.mct = {
      enable = true;
      hook = ["(on-first-input . mct-mode)"];
      custom = {
	mct-hide-completion-mode-line = lib.mkDefault true;
	mct-completing-read-multiple-indicator = lib.mkDefault true;
	mct-live-completion = lib.mkDefault "'visible";
      };
    };
  };
}
