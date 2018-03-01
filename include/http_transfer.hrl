%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 三月 2018 下午2:52
%%%-------------------------------------------------------------------
-author("yangyajun03").

-define(APPNAME, 'http_transfer').
-define(COWBOY_REF, 'http').

-define(OPTION, [{response_format, binary},
    {max_sessions, 10}, {max_pipeline_size, 100}  ]).
-define(TIMEOUT, 3000).