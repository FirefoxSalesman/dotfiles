{ inputs, ... }:

{
  perSystem = { pkgs, self', ... }: {
    packages.wiki = pkgs.writeShellScriptBin "wiki" ''
      dir="/usr/share/doc/arch-wiki/html/en/"
      doc="$(${pkgs.findutils}/bin/find $dir -iname "*.html" | \
      ${pkgs.coreutils}/bin/cut -d '/' -f8- | \
      ${self'.packages.ezf}/bin/ezf)"

      if [ "''${dir}$doc" ]; then
        emacsclient -e "(eww-open-file \"''${dir}$doc\")"
      else
        ${pkgs.coreutils}/bin/echo "Program terminated." && exit 0
      fi
    '';
  };
}
