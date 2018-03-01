src/ibrowse.erl:: include/ibrowse.hrl src/ibrowse_lib.erl; @touch $@
src/ibrowse_http_client.erl:: include/ibrowse.hrl src/ibrowse_lib.erl; @touch $@
src/ibrowse_lb.erl:: include/ibrowse.hrl; @touch $@
src/ibrowse_lib.erl:: include/ibrowse.hrl; @touch $@
src/ibrowse_socks5.erl:: src/ibrowse_lib.erl; @touch $@

COMPILE_FIRST += ibrowse_lib
