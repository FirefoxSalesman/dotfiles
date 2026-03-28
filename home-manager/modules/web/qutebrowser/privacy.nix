{
  flake.homeModules.web = { ... }: {
    programs.qutebrowser.settings = {
      content = {
	blocking = {
	  enabled = true;
	  method = "both";
	  adblock.lists = [
	    "https://easylist.to/easylist/easylist.txt"
	    "https://easylist.to/easylist/easyprivacy.txt"
	    "https://easylist.to/easylist/fanboy-social.txt"
	    "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
	    "https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt"
	    #"https://gitlab.com/curben/urlhaus-filter/-/raw/master/urlhaus-filter.txt"
	    "https://pgl.yoyo.org/adservers/serverlist.php?showintro=0;hostformat=hosts"
	    "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/NorwegianExperimentalList%20alternate%20versions/NordicFiltersABP-Inclusion.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt"
	    "https://www.i-dont-care-about-cookies.eu/abp/"
	    "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
	    "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt"
	    "https://raw.githubusercontent.com/uBlockOrigin/uAssets/master/filters/quick-fixes.txt"
	  ];
	};
	autoplay = false;
	cookies.store = false;
	geolocation = false;
	private_browsing = true;
      };
      completion = {
	cmd_history_max_items = 0;
	web_history.max_items = 0;
      };
    };
  };
}
