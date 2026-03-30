{
  flake.homeModules.web = { config, pkgs, ... }:

  {
    home.packages = with pkgs; [
      python313Packages.adblock
      libfido2
      yubikey-manager
    ];

    programs.qutebrowser = {
      enable = true;
      enableDefaultBindings = true;
      package = (config.lib.nixGL.wrap pkgs.qutebrowser);
      loadAutoconfig = false;

      aliases = {
	"q" = "quit";
	"w" = "session-save";
	"wq" = "quit --save";
      };

      searchEngines = {
	"DEFAULT" = "https://duckduckgo.com/?q={}";
      };
  
      settings = {
	downloads.location.directory = "~/dwn";
	url = {
	  default_page = "https://search.inetol.net";
	  start_pages = "https://search.inetol.net";
	};
	hints.chars = "crstbfneia";
      };

      extraConfig = ''
config.set('content.cookies.accept', 'no-3rdparty', 'chrome-devtools://*')
config.set('content.cookies.accept', 'no-3rdparty', 'devtools://*')

config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

config.set('content.notifications.enabled', False, 'https://www.reddit.com')
config.set('content.notifications.enabled', False, 'https://www.youtube.com')

c.content.headers.user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:144.0) Gecko/20100101 Firefox/144.0'

c.editor.command = ['emacsclient', '{file}']

c.fonts.default_family = '"Source Code Pro"'
c.fonts.default_size = '8pt'
c.fonts.completion.entry = '8pt "Source Code Pro"'
c.fonts.debug_console = '8pt "Source Code Pro"'
c.fonts.prompts = 'default_size sans-serif'
      '';
    };
  };
}
