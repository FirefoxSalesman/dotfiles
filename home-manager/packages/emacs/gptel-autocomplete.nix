  { inputs, trivialBuild, gptel } :

  trivialBuild rec {
    pname = "gptel-autocomplete";
    version = "current";
    src = inputs.gptel-autocomplete;

    propagatedUserEnvPkgs = [
      gptel
    ];

    buildInputs = propagatedUserEnvPkgs;
  }
