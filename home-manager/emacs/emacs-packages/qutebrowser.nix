{ trivialBuild, inputs, consult, exwm, password-store, dash, evil } :

trivialBuild rec {
  pname = "qutebrowser";
  version = "current";
  src = inputs.exwm-qutebrowser;

  propagatedUserEnvPkgs = [
    consult
    exwm
    password-store
    dash
    evil
  ];

  buildInputs = propagatedUserEnvPkgs;
}
