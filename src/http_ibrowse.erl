%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 三月 2018 下午3:07
%%%-------------------------------------------------------------------
-module(http_ibrowse).
-author("yangyajun03").

%% API
-export([ibrowse_send/6]).

ibrowse_send(Method, Url, Headers, Body, Options, Timeout) ->
    case ibrowse:send_req(Url, Headers, Method, Body, Options, Timeout) of
        {ok, "200", _ResponseHeaders, ResponseBody} ->
            lager:info("ibrowse_send url:~p, head:~p, reqbody:~p, result:~p",
                [Url, Headers, Body, ResponseBody]),
            {200, ResponseBody};
        {ok, Code, _ResponseHeaders, ResponseBody} ->
            lager:error("ibrowse_send code:~p, url:~p, head:~p, reqbody:~p, result:~p",
                [Code, Url, Headers, Body, ResponseBody]),
            {list_to_integer(Code), ResponseBody};
        {error, Error} ->
            lager:error("ibrowse_send url:~p, head:~p, reqbody:~p, error:~p",
                [Url, Headers, Body, Error]),
            {500, Error}
    end.