  { trivialBuild, inputs } :

  trivialBuild rec {
    pname = "app-launcher";
    version = "current";
    src = inputs.app-launcher;
  }
