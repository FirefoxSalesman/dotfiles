{ trivialBuild, inputs } :

trivialBuild rec {
  pname = "mpc-wrapper";
  version = "current";
  src = inputs.mpc-wrapper;
}
