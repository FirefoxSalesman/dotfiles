{ inputs, ... }:

{
  perSystem = { pkgs, ... }:
  let epkgs = pkgs.emacs.pkgs;
  in {
    packages.embark = epkgs.callPackage (
      {
        org,
        consult,
        avy,
        compat,
        elpaBuild,
        fetchurl,
        lib,
      }:
      elpaBuild {
        pname = "embark";
        ename = "embark";
        version = "1.1";
        src = fetchurl {
	  url = "https://elpa.gnu.org/packages/embark-1.1.tar";
	  sha256 = "074ggh7dkr5jdkwcndl6znhkq48jmc62rp7mc6vjidr6yxf8d1rn";
        };
        packageRequires = [
	  org
	  consult
	  avy
	  compat
        ];
        meta = {
	  homepage = "https://elpa.gnu.org/packages/embark.html";
	  license = lib.licenses.free;
        };
      }
    ) { };
  };
}
