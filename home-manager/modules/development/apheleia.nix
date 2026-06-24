{ ... }:

{
  flake.homeModules.development =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    let
      tools = config.programs.emacs.init.tools;
      makeModeFormatters =
        vs:
        lib.concatStringsSep "\n" (
          lib.optionals (vs != { }) (lib.mapAttrsToList (mode: formatter: "(${mode} . ${formatter})") vs)
        );
      makeFormattersHelper =
        formatter:
        if lib.isList formatter then "'(${lib.concatMapStrings (k: ''"${k}" '') formatter})" else formatter;
      makeFormatters =
        vs:
        lib.concatStringsSep "\n" (
          lib.optionals (vs != { }) (
            lib.mapAttrsToList (
              name: formatter: "(setf (alist-get '${name} apheleia-formatters) ${makeFormattersHelper formatter})"
            ) vs
          )
        );
    in
    {
      options.programs.emacs.init.tools.apheleia = {
        autoFormat = lib.mkEnableOption "Auto-format buffers on save with apheleia.";
        modeFormatters = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          description = "Aphelia formatters for each major mode";
        };
        formatters = lib.mkOption {
          type = lib.types.attrsOf (lib.types.either lib.types.str (lib.types.listOf lib.types.str));
          default = { };
          description = "Aphelia formatters & their commands";
        };
      };

      config.programs.emacs.init.usePackage.apheleia = lib.mkIf (tools.apheleia.formatters != { }) {
        enable = true;
        ghookf = lib.mkIf tools.apheleia.autoFormat [ "('on-first-file 'apheleia-global-mode)" ];
        config = ''
          ${makeFormatters tools.apheleia.formatters}

          (dolist (pair '(${makeModeFormatters tools.apheleia.modeFormatters}))
            (add-to-list 'apheleia-mode-alist pair))
        '';
        generalOne.global-leader."F" =
          lib.mkIf config.programs.emacs.init.keybinds.leader-key.enable "'apheleia-format-buffer";
      };
    };
}
