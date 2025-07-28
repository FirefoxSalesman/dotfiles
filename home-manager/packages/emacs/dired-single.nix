  { trivialBuild, inputs } :

  trivialBuild rec {
    pname = "dired-single";
    version = "current";
    src = inputs.dired-single;
  }
