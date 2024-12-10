{ inputs, trivialBuild, ox-pandoc } :

trivialBuild rec {
  pname = "org-modern-indent";
  version = "current";
  src = inputs.org-auto-export-pandoc;

  propagatedUserEnvPkgs = [
    ox-pandoc
  ];

  buildInputs = propagatedUserEnvPkgs;
}
