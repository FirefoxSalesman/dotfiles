{ trivialBuild, inputs } :

trivialBuild rec {
  pname = "repeaters";
  version = "current";
  src = inputs.repeaters;
}
