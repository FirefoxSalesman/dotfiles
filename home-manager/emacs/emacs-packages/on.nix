{ trivialBuild, inputs } :

trivialBuild rec {
  pname = "on";
  version = "current";
  src = inputs.on-el;
}
