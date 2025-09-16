{ inputs, trivialBuild, gptel } :

trivialBuild rec {
  pname = "macher";
  version = "current";
  src = inputs.macher;

  propagatedUserEnvPkgs = [
    gptel
  ];

  buildInputs = propagatedUserEnvPkgs;
}
