{ inputs, trivialBuild, gptel, orderless } :

trivialBuild rec {
  pname = "ragmacs";
  version = "current";
  src = inputs.ragmacs;

  propagatedUserEnvPkgs = [
    orderless
    gptel
  ];

  buildInputs = propagatedUserEnvPkgs;
}
