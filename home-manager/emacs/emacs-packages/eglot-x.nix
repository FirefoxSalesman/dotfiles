{ inputs, trivialBuild, eglot  } :

trivialBuild rec {
  pname = "eglot-booster";
  version = "current";
  src = inputs.eglot-booster;

  propagatedUserEnvPkgs = [
    eglot
  ];

  buildInputs = propagatedUserEnvPkgs;
}
