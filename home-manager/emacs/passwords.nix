{ pkgs, ... }:

{
  home.packages = with pkgs; [ (pass.withExtensions (exts:
    with exts; [ pass-otp ]
  ))];

  programs.emacs.init.usePackage = {
    password-store = {
      enable = true;
      defer = true;
      generalOne.global-leader = {
        "p" = '''(:ignore t :which-key "pass")'';
        "py" = '''("yank" . password-store-copy)'';
        "pi" = '''("insert" . password-store-insert)'';
        "pg" = '''("generate" . password-store-generate)'';
        "pe" = '''("edit" . password-store-edit)'';
        "pd" = '''("delete" . password-store-delete)'';
      };
      extraConfig = ":autoload pass-entries password-store-list";
    };
    
    password-store-otp = {
      enable = true;
      defer = true;
      generalOne.global-leader."po" = '''("copy otp" . password-store-otp-token-copy)'';
    };
    
    auth-source-pass = {
      enable = true;
      after = ["magit"];
      config = "(auth-source-pass-enable)";
    };
    
    pinentry = {
      enable = true;
      setopt.epa-pinentry-mode = "'loopback";
      deferIncrementally = true;
      config = "(pinentry-start)";
    };
  };
}
