{
  flake.homeModules.web = { ... }:
  {
    programs.emacs.init.usePackage.elfeed = {
      enable = true;
      defer = true;
      setopt.elfeed-feeds = [
	''"https://lukesmith.xyz/index.xml"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UCSJPFQdZwrOutnmSFYtbstA"''
	''"https://planet.emacslife.com/atom.xml"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UC_GQ4mac4oN3wl1UdbFuTEA"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UC6UBbvEA8uh6Ulc6ax1Zs0g"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UCNzZD3otfZVlIdvYYRRqNSw"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UCnnkTXnyn0uZzmArZO99Klg"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UCq-VIBjS6Ia1r1IR_j-7NUw"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UC0E_vIe1e1lVeojYOgVg_5Q"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UCUQs6rEz6lRGHn6DWqss0hA"''
	''"https://youtube.com/feeds/videos.xml?channel_id=UC_zBdZ0_H_jn41FDRG7q4Tw"''
	''"https://notrelated.xyz/rss"''
      ];
      generalOne.global-leader."r" = ''`("elfeed" . ,(cmd! (elfeed) (elfeed-update)))'';
    };
  };
}
