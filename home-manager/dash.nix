{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.programs.dash;

  writeBashScript = name: text:
    pkgs.writeTextFile {
      inherit name text;
      checkPhase = ''
        ${pkgs.stdenv.shellDryRun} "$target"
      '';
    };

in {
  meta.maintainers = [ maintainers.rycee ];
  options = {
    programs.dash = {
      enable = mkEnableOption "Debian Almquiest Shell";
  
      shellOptions = mkOption {
        type = types.listOf types.str;
        default = [];
        example = [ "extglob" "-cdspell" ];
        description = ''
          Shell options to set. Prefix an option with
          "`-`" to unset.
        '';
      };
  
      sessionVariables = mkOption {
        default = { };
        type = types.attrs;
        example = { MAILCHECK = 30; };
        description = ''
          Environment variables that will be set for the dash session.
        '';
      };
  
      shellAliases = mkOption {
        default = { };
        type = types.attrsOf types.str;
        example = literalExpression ''
          {
            ll = "ls -l";
            ".." = "cd ..";
          }
        '';
        description = ''
          An attribute set that maps aliases (the top level attribute names in
          this option) to command strings or directly to build outputs.
        '';
      };
  
      profileExtra = mkOption {
        default = "";
        type = types.lines;
        description = ''
          Extra commands that should be run when initializing a login
          shell.
        '';
      };
  
      initExtra = mkOption {
        default = "";
        type = types.lines;
        description = ''
          Extra commands that should be run when initializing an
          interactive shell.
        '';
      };
  
      dashrcExtra = mkOption {
        default = "";
        type = types.lines;
        description = ''
          Extra commands that should be placed in {file}`~/.dashrc`.
          Note that these commands will be run even in non-interactive shells.
        '';
      };
    };
  };

  config = let
    aliasesStr = concatStringsSep "\n"
      ((mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}")
        cfg.shellAliases) ++ 
      (mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}")
        config.home.shellAliases));
  
    globalAliasesStr = concatStringsSep "\n"
      (mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}")
        home.shellAliases);
  
    shoptsStr = let switch = v: if hasPrefix "-" v then "-u" else "-s";
    in concatStringsSep "\n"
    (map (v: "shopt ${switch v} ${removePrefix "-" v}") cfg.shellOptions);
  
    sessionVarsStr = config.lib.shell.exportAll cfg.sessionVariables;
  
  in mkIf cfg.enable {
    home.file.".profile".source = lib.mkForce (writeBashScript "profile" ''
      . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"
  
      ${sessionVarsStr}
  
      ${cfg.profileExtra}
    '');
  
    home.file.".dashrc".source = writeBashScript "dashrc" ''
      ${cfg.dashrcExtra}
  
      ${shoptsStr}
  
      ${aliasesStr}
  
      ${cfg.initExtra}
    '' ;
  
    home.sessionVariables.ENV = "$HOME/.dashrc";
  };
}
