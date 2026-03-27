{
  flake.homeModules.ledger = { ... }:
  {
    programs.emacs.init = {
      completions.tempel.templates.ledger-mode = {
	payroll = ''p "Rose-Hulman Payroll" n> "Income:TeachingAssistant" > "-" p n> "*Assets:Checking"'';
	checking = ''"Assets:Checking"'';
      };
      ide.languages.ledger.enable = true;
    };
  };
}
