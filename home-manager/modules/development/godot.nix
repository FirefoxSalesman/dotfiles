{
  flake.homeModules.godot = { pkgs, ... }:
  {
    programs.emacs.init = {
      ide.treesitter.treesitterGrammars.gdscript = "https://github.com/PrestonKnopp/tree-sitter-gdscript.git";
      usePackage.gdscript-ts-mode = {
	enable = true;
	package = epkgs: epkgs.gdscript-mode;
	extraPackages = [pkgs.godot pkgs.gdtoolkit_4];
	symex = true;
	eglot = true;
        mode = [ ''"\\.tscn\\'"'' ''"\\.gd\\'"''] ;
      };
    };
  };
}
