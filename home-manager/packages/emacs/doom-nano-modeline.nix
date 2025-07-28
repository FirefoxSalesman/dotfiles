  { inputs, trivialBuild, doom-themes } :

  trivialBuild rec {
    pname = "doom-nano-modeline";
    version = "current";
    src = inputs.doom-nano-modeline;

    propagatedUserEnvPkgs = [
      doom-themes
    ];

    buildInputs = propagatedUserEnvPkgs;
  }
