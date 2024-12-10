{ trivialBuild, inputs } :

trivialBuild rec {
  pname = "ezf";
  version = "current";
  src = inputs.ezf;
}
