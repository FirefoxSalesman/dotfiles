{ trivialBuild, inputs, consult, request, plz } :

trivialBuild rec {
  pname = "consult-omni";
  version = "current";
  src = inputs.consult-omni;

  propagatedUserEnvPkgs = [
    consult
    request
    plz
  ];

  buildInputs = propagatedUserEnvPkgs;
}
