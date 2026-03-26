{ pkgs }:

pkgs.writeShellScriptBin "emacsclient-wrapper" ''
  emacsclient -e "(let ((qutebrowser-fifo \"$QUTE_FIFO\")
                        (qutebrowser-command-backend 'qutebrowser-fifo-send))
                    $@)"; true
''
