{ inputs, ... }:

{
  flake.homeModules.chat =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [
        thunderbird
      ];

      programs.vesktop = {
        enable = true;
        settings = {
          minimizeToTray = false;
          tray = false;
          hardwareAcceleration = true;
        };
        vencord = {
          useSystem = true;
          settings = {
            autoUpdate = false;
            autoUpdateNotification = false;
            plugins = {
              FakeNitro.enabled = true;
              AnonymizeFileNames.enabled = true;
              BetterSettings.enabled = true;
              FixYoutubeEmbeds.enabled = true;
              ImageZoom.enabled = true;
              YoutubeAdblock.enabled = true;
            };
          };
        };
      };
    };
}
