{
  flake.homeModules.shellConfig.home.sessionVariables = {
    _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java";
    GTK2_RC_FILES = "/home/holschcc/.gtkrc-2.0";
    SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    PATH = "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/holschcc/.local/bin:/home/holschcc/.nix-profile/bin";
    OLLAMA_API_BASE = "http://127.0.0.1:11434";
    GEMINI_API_KEY = "TERM"; # This makes minuet happy
  };
}
