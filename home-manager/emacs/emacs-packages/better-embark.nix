{ inputs, trivialBuild, org, compat, consult, avy } :

trivialBuild rec {
  pname = "better-embark";
  version = "current";
  src = inputs.emacs-embark;

  propagatedUserEnvPkgs = [
    org
    compat
    consult
    avy
  ];

  buildInputs = propagatedUserEnvPkgs;
}
