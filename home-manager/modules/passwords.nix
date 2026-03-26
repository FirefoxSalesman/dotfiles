{
  flake.homeModules.passwordManagement = { pkgs, ... }:

  {
    home.packages = with pkgs; [ (pass.withExtensions (exts:
      with exts; [ pass-otp ]
    ))];

    home.file.".local/share/gnupg/gpg-agent.conf".text = ''
       pinentry-program /usr/bin/pinentry-emacs
       allow-loopback-pinentry
       allow-emacs-pinentry
       default-cache-ttl 600
       max-cache-ttl 7200
       enable-ssh-support
    '';
    home.file.".local/share/gnupg/gpg.conf".text = ''
       use-agent
    '';

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
  };
}
