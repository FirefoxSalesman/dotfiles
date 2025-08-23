{ pkgs, config, lib, ... }:

let
  ide = config.programs.emacs.init.ide;
in
{
  options.programs.emacs.init.ide.languages.julia.enable = lib.mkEnableOption "Enables support for julia (stolen from doom). No support for lspce.";

  config.programs.emacs.init.usePackage = lib.mkIf ide.languages.julia.enable {
    julia-ts-mode = {
      enable = true;
      mode = [''"\\.jl\\'"''];
      lsp = ide.lsp.enable;
      eglot = ide.eglot.enable;
    };

    julia-repl = {
      enable = true;
      hook = ["(julia-ts-mode . julia-repl-mode)"];
    };

    eglot-jl = lib.mkIf ide.eglot.enable {
      enable = true;
      after = ["eglot"];
      hook = ["(julia-ts-mode . (lambda () (setq-local eglot-connect-timeout (max eglot-connect-timeout 60))))"];
    };

    lsp-julia = lib.mkIf ide.lsp.enable {
      enable = true;
      after = ["lsp-mode"];
      config = ''
        (add-to-list 'lsp-language-id-configuration '(julia-ts-mode . "julia"))
        (lsp-register-client
          (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-julia--rls-command)
                           :major-modes '(julia-mode ess-julia-mode julia-ts-mode)
                           :server-id 'julia-ls
                           :multi-root t))
      '';
    };
  };
}
