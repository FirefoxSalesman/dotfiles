{
  flake.homeModules.emacs = {
    programs.emacs.init.usePackage.evil-exchange = {
      enable = true;
      generalOne = {
        evil-operator-state-map."k" = "'evil-exchange/cx";
        evil-visual-state-map."k" = "'evil-exchange";
      };
      hook = [
        {
          namesDeferred = [ "doom-escape" ];
          actions = [ "(lambda () (when (featurep 'evil-exchange) (evil-exchange-cancel)))" ];
        }
      ];
    };
  };
}
