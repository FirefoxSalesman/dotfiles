{
  flake.homeModules.passwordManagement = { ... }:
  {
    home.file = {
      ".local/share/gnupg/gpg-agent.conf".text = ''
	pinentry-program /usr/bin/pinentry-emacs
        allow-loopback-pinentry
        allow-emacs-pinentry
        default-cache-ttl 600
        max-cache-ttl 7200
        enable-ssh-support
      '';
      ".local/share/gnupg/gpg.conf".text = ''
	use-agent
      '';
    };
    programs.emacs.init.usePackage.pinentry = {
	enable = true;
	setopt.epa-pinentry-mode = "'loopback";
	deferIncrementally = true;
	config = "(pinentry-start)";
    };
  };
}
