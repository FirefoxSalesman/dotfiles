{
  flake.homeModules.web = { ... }: {
    programs.emacs.init.usePackage.webjump = {
      enable = true;
      generalOne.global-leader."s" = "'webjump";
      setopt.webjump-sites = [
	'''("DuckDuckGo" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])''
	'''("Invidious" . [simple-query "inv.nadeko.net" "inv.nadeko.net/search?q=" ""])''
	'''("Aur" . [simple-query "aur.archlinux.org" "aur.archlinux.org/packages/?K=" ""])''
	'''("Nixpkgs" . [simple-query "search.nixos.org" "search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=" ""])''
	'''("Curseforge" . webjump-to-curseforge)''
	'''("Modrinth" . webjump-to-modrinth)''
      ];
      config = ''
	(defun webjump-to-curseforge (name)
	  "Webjump function for searching curseforge.
	NAME is the term to search for."
	  (let* ((prefix "legacy.curseforge.com/minecraft/")
	         (category (completing-read "Choose a category" '("mc-mods" "modpacks" "shaders" "data-pack" "texture-packs")))
		 (url (concat prefix category))
		 (term (webjump-read-string (concat name " Search for"))))
	    (concat url "/search?search=" (webjump-url-encode term))))
	
	(defun webjump-to-modrinth (name)
	  "Webjump function for searching modrinth.
	NAME is the term to search for."
	  (let* ((prefix "modrinth.com/")
	         (category (completing-read "Choose a category" '("mods" "resourcepacks" "datapacks" "shaders" "modpacks" "plugins")))
		 (url (concat prefix category))
		 (term (webjump-read-string (concat name " Search for"))))
	    (concat url "?q=" (webjump-url-encode term))))
      '';
    };
  };
}
