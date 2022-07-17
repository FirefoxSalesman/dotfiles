#!/usr/bin/env sh
# AUTHOR: gotbletu (@gmail|twitter|youtube|github|lbry|odysee)
#         https://www.youtube.com/user/gotbletu
# DESC:   w3m search engine alias (aka omnibar smart keywords search)
# DEMO:   https://youtu.be/bWlPpacFPlI
#         W3M Playlist https://www.youtube.com/playlist?list=PLqv94xWU9zZ35Yv0s6zMID5JoS8qu19Kh
# REFF:   https://github.com/felipesaa/A-vim-like-firefox-like-configuration-for-w3m/blob/master/cgi-bin/search_engines.cgi
#         https://github.com/felipesaa/A-vim-like-firefox-like-configuration-for-w3m/blob/master/documentation/search_engines.txt
#         https://github.com/felipesaa/A-vim-like-firefox-like-configuration-for-w3m/blob/master/urimethodmap
#         frakswe search surfraw urimethodmap https://pastebin.com/raw/TUcRgu9y
#         https://rubikitch.hatenadiary.org/entry/20070830/searchengine
# REQD:   1. touch ~/.w3m/urimethodmap
#         2. $EDITOR ~/.w3m/urimethodmap
#             # search engine alias
#             # note: no trailing space after %s
#             1x: file:/cgi-bin/search_engines.cgi?%s
#             dd: file:/cgi-bin/search_engines.cgi?%s
#             rd: file:/cgi-bin/search_engines.cgi?%s
#             wi: file:/cgi-bin/search_engines.cgi?%s
#             yt: file:/cgi-bin/search_engines.cgi?%s
#         3. chmod +x ~/.w3m/cgi-bin/search_engines.cgi
#         4. sed -i 's@cgi_bin.*@cgi_bin ~/.w3m/cgi-bin:/usr/lib/w3m/cgi-bin:/usr/local/libexec/w3m/cgi-bin@g' ~/.w3m/config
#         5. sed -i 's@urimethodmap.*@urimethodmap ~/.w3m/urimethodmap, /usr/etc/w3m/urimethodmap@g' ~/.w3m/config
#         6. sed -i 's@space_autocomplete.*@space_autocomplete 0@g' ~/.w3m/config
# USAGE:  <SearchEngine>:<Keywords>
#         1. $ w3m -v
#         2. press Shift-u (the default GOTO key) to access the addressbar
#         3. press Ctrl-u to clear addressbar
#         4. to search searx type in sx:cool linux wallpapers
#         5. to search wikipedia type in wi:free sand
# CLI:    $ w3m yt:luke,smith
# CLOG:   
#         2021-07-28 add comma as space holder for cli (eg $ w3m ya:debian,stable,iso )
#         2021-07-24 remove semicolon requirements, use spaces instead by enabling (space_autocomplete 0)
#                    add optional direct cli usage info
#         2021-05-24 turn @felipesaa script into posix, use case statement looks prettier
#	  2022-07-13 holscher ruins the script by onionizing everything

# search engine alias
PREFIX=$(echo "$QUERY_STRING" | cut -d ':' -f1)

# user input keywords
INPUT=$(echo "$QUERY_STRING" | cut -d ':' -f2- | sed 's/,/%20/g')

# check if w3m version has native gopher support
GOPHER_PROTOCOL_ENABLE=$(w3m -version | grep -c "gopher")

case $PREFIX in
  lb) # goto a library channel by name
    echo "W3m-control: GOTO http://librarian.lqs5fjmajyp7rvp4qvyubwofzi6d4imua7vs237rkc4m5qogitqwrgyd.onion/@$INPUT"
    ;;
  rd) # goto a subreddit by name
    echo "W3m-control: GOTO http://ecue64ybzvn6vjzl37kcsnwt4ycmbsyf74nbttyg7rkc3t3qwnj7mcyd.onion/r/$INPUT/"
    ;;
  sx) # searx northboot
    echo "W3m-control: GOTO http://4n53nafyi77iplnbrpmxnp3x4exbswwxigujaxy3b37fvr7bvlopxeyd.onion/search?q=$INPUT/"
    ;;
  wi) # wikipedia through northboot wikiless
    echo "W3m-control: GOTO http://ybgg2evrcdz37y2qes23ff3wjqjdn33tthgoagi76vhxytu4mpxiz5qd.onion/w/index.php?search=$INPUT&title=Special%3ASearch&profile=default&fulltext=1&ns0=1"
    ;;
  yt) # youtube via invidious (more instances @ https://redirect.invidious.io )
    echo "W3m-control: GOTO http://c7hqkpkpemu6e7emz5b4vyz7idjgdvgaaa3dyimmeojqbgpea3xqjoid.onion/search?q=$INPUT"
    ;;
esac

# delete temp buffer
echo "W3m-control: DELETE_PREVBUF"
