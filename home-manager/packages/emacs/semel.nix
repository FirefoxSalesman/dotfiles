{ trivialBuild, inputs } :

trivialBuild rec {
  pname = "semel";
  version = "current";
  src = inputs.semel;
}
