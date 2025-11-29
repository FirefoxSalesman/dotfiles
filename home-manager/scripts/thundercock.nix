{ pkgs }:

pkgs.writeShellScriptBin "thundercock" ''
  ${pkgs.thunderbird}/bin/thunderbird --headless &
  sleep 60
  pkill thunderbird
  cp -r ~/.thunderbird/l7564xpl.default/ImapMail/outlook.office365.com/INBOX-1 ~/.thunderbird/l7564xpl.default/ImapMail/outlook.office365.com/INBOX
  ${pkgs.mu}/bin/mu index
''
