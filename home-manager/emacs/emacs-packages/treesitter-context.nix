{ inputs, trivialBuild, posframe } :

trivialBuild rec {
  pname = "treesitter-context";
  version = "current";
  src = inputs.treesitter-context;

  propagatedUserEnvPkgs = [
    posframe
  ];

  buildInputs = propagatedUserEnvPkgs;
}
