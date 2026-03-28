{
  flake.homeModules.web = { ... }:
  {
    programs.qutebrowser.keyBindings = {
      normal = {
	"m" = "search-next";
	"M" = "search-prev";
	"e" = "fake-key <Down>";
	"o" = "fake-key <Up>";
	"O" = "scroll-page 0 -0.5";
	"E" = "scroll-page 0 0.5";
	"<ctrl-o>" = "scroll-page 0 -1";
	"<ctrl-e>" = "scroll-page 0 1";
	"t" = "mode-enter insert";
	"I" = "forward";
	"i" = "fake-key <Right>";
	"n" = "fake-key <Left>";
	"N" = "back";
	"h" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher)'";
	"H" = "spawn --userscript emacsclient-wrapper '(qutebrowser-launcher-tab)'";
	";l" = "spawn --userscript qute-pass";
	";u" = "spawn --userscript qute-pass --username-only";
	";p" = "spawn --userscript qute-pass --password-only";
	";o" = "spawn --userscript qute-pass --otp-only";
	";P" = "spawn --userscript emacsclient-wrapper '(qutebrowser-pass \"{url}\")'";
	"a" = "cmd-set-text :";
	"k" = "hint links spawn --userscript emacsclient-wrapper '(mpv-play-url \"{hint-url}\")'";
      };
    };
  };
}
