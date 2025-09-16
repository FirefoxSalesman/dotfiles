{ pkgs, ... }:

{
  home.packages = with pkgs; [ pass ];

  programs.emacs.init.usePackage = {
    password-store = {
      enable = true;
      defer = true;
      generalOne.global-leader = {
        "p" = '''(:ignore t :which-key "pass")'';
        "py" = '''(password-store-copy :which-key "yank")'';
        "pi" = '''(password-store-insert :which-key "insert")'';
        "pg" = '''(password-store-generate :which-key "generate")'';
      };
      extraConfig = ":autoload pass-entries password-store-list";
    };
    
    password-store-otp = {
      enable = true;
      defer = true;
      generalOne.global-leader."po" = '''(password-store-otp-token-copy :which-key "copy otp")'';
    };
    
    pinentry = {
      enable = true;
      custom.epa-pinentry-mode = "'loopback";
      deferIncrementally = true;
      config = ''
        (pinentry-start)
        (shell-command "gpgconf --launch-agent")
        (shell-command "gpg-connect agent updatestartuptty /bye >/dev/null")
      '';
    };
  };
}
