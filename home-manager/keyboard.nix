  { ... }:

  {
    home.file = {
      ".config/kmonad/kmonad.kbd".text = ''
        ;; ** For Linux **
        (defcfg
        input  (device-file "/dev/input/by-path/platform-i8042-serio-0-event-kbd") ;; Change this line for your device
        output (uinput-sink "KMonad output")

        fallthrough true)

        (defsrc
          esc     f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12            prnt    slck    pause
          grv     1    2    3    4    5    6    7    8    9    0    -    =    bspc      ins     home    pgup
          tab     q    w    e    r    t    y    u    i    o    p    [    ]    \         del     end     pgdn
          caps    a    s    d    f    g    h    j    k    l    ;    '    ret
          lsft    z    x    c    v    b    n    m    ,    .    /    rsft                         up
          lctl    lmet lalt           spc            ralt    rctl                       left    down    right)

        (defalias
          def  (tap-macro nlck (layer-switch canary)) ;; Bind 'def' to canary Layer
          caps  (tap-macro nlck (layer-switch canary-shift)) ;; Bind 'def' to canary Layer
          cesc (tap-hold 200 esc (layer-toggle extend))
          sesc (tap-hold 200 S-esc (layer-toggle extend))
          num (sticky-key 500 (layer-toggle num))
          qwerty (tap-macro nlck (layer-switch qwerty)) ;; Bind 'def' to canary Layer
          smet (sticky-key 500 lmet)
          salt (sticky-key 500 lalt)
          sshft (sticky-key 500 lshft)
          spc (tap-hold 200 spc caps)
          SPC (tap-hold 200 S-spc caps)
          c (tap-hold 200 c lmet)
          r (tap-hold 200 r lalt)
          s (tap-hold 200 s lctl)
          t (tap-hold 200 t (layer-toggle canary-shift))
          a (tap-hold 200 a lmet)
          i (tap-hold 200 i lalt)
          e (tap-hold 200 e lctl)
          n (tap-hold 200 n (layer-toggle canary-shift))
          C (tap-hold 200 C lmet)
          R (tap-hold 200 R lalt)
          S (tap-hold 200 S lctl)
          A (tap-hold 200 A lmet)
          I (tap-hold 200 I lalt)
          E (tap-hold 200 E lctl)
          wlft C-left
          wrht C-right
          bhst A-left
          fhst A-right)
        (deflayer canary
          esc     f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12            prnt    slck    pause
          $       &    [    {    }    \(   =    *    \)   +    ]    !    `    bspc      ins     home    pgup
          tab     w    l    y    p    k    z    x    o    u    ;    -    @    \         del     end     pgdn
          @cesc   @c   @r   @s   @t   b    f    @n   @e   @i   @a   '    ret
          shft   j    v    d    g    q    m    h    /    ,    .    @sshft                      up
          caps   @smet @num           @spc             caps     rctl                    left    down    right)

        (deflayer canary-shift
          S-esc   S-f1 S-f2 S-f3 S-f4 S-f5 S-f6 S-f7 S-f8 S-f9 S-f10 S-f11 S-f12        S-prnt  S-slck  S-pause
          ~       %    7    5    3    1    9    0    2    4    6    8    #  S-bspc      S-ins   S-home  S-pgup
          S-tab   W    L    Y    P    K    Z    X    O    U    :    \_   ^    S-\        S-del   S-end   S-pgdn
          @sesc   @C   @R   @S   T    B    F    N   @E   @I   @A    "    S-ret
          @sshft   J    V    D    G    Q    M    H    ?    <    >    @sshft                     S-up
          caps   @smet @def           @SPC             caps    rctl                   S-left S-down S-right)

        (deflayer qwerty
          esc     f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12            prnt    slck    pause
          grv     1    2    3    4    5    6    7    8    9    0    -    =    bspc      ins     home    pgup
          tab     q    w    e    r    t    y    u    i    o    p    [    ]    \         del     end     pgdn
          caps    a    s    d    f    g    h    j    k    l    ;    '    ret
          lsft    z    x    c    v    b    n    m    ,    .    /    rsft                         up
          lctl    lmet lalt           spc            @def     rctl                     left    down    right)

        (deflayer extend
          _       _    _    _    _    _    _    _    _    _    _    _    _              _    _    _
          _     f11    f7   f5   f3   f1   f9  f10   f2   f4   f6   f8  f12   _         _    _    _
          _   @bhst @fhst  C-z  C-x   C-c  C-z @wlft up  @wrht  _    _    _    _         _    _    _
          _   @smet @salt lctl @sshft del bspc left down right _    _    _
          _         _    _  @caps    _    _    _    h    _    _    _    _                      _
          _       _    _               ret              @qwerty        _                     _    _    _)
      
        (deflayer num
          _       _    _    _    _    _    _    _    _    _    _    _    _              _    _    _
          _     f11    f7   f5   f3   f1   f9  f10   f2   f4   f6   f8  f12   _         _    _    _
          _       \_    =    +    -    [    ]    7    8    9   &    ^    _    _         _    _    _
          _   @smet @salt lctl @sshft  {    }    4    5    6   0    *    _
          _        @    !    $     \(  ~  \)    1    2    3    #    _                      _
          _       _    _               ret              _        _                     _    _    _)
      '';
        
      ".Xmodmap".text = ''
        clear lock
        clear mod3
        clear mod4
        keycode 66 = Hyper_R
        keycode 105 = Caps_Lock
        add mod3 = Hyper_L Hyper_R
        add mod4 = Super_L Super_R
      '';
    };
  }
