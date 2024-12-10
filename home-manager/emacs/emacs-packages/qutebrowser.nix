{ trivialBuild, inputs, consult, exwm, password-store, dash } :

trivialBuild rec {
  pname = "qutebrowser";
  version = "current";
  src = inputs.exwm-qutebrowser;

  propagatedUserEnvPkgs = [
    consult
    exwm
    password-store
    dash
  ];

  buildInputs = propagatedUserEnvPkgs;
}
