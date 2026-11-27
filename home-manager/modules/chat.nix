{
  flake.homeModules = {
    chat =
      { pkgs, ... }:
      {
        home.packages = [ pkgs.thunderbird ];
      };
    personalChat.programs.vesktop = {
      enable = true;
      settings = {
        minimizeToTray = false;
        tray = false;
        hardwareAcceleration = true;
      };
      vencord.settings = {
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
}
