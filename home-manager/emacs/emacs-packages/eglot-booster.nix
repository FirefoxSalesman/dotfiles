{ inputs, trivialBuild, eglot, emacs-lsp-booster, jsonrpc } :

trivialBuild rec {
  pname = "eglot-booster";
  version = "current";
  src = inputs.eglot-booster;

  propagatedUserEnvPkgs = [
    eglot
    emacs-lsp-booster
    jsonrpc
  ];

  buildInputs = propagatedUserEnvPkgs;
}
