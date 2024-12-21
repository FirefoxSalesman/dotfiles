{ inputs, trivialBuild, project } :

trivialBuild rec {
  pname = "disproject";
  version = "current";
  src = inputs.disproject;

  propagatedUserEnvPkgs = [
    project
  ];

  buildInputs = propagatedUserEnvPkgs;
}
