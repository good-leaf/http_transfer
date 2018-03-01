-module(http_transfer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-include("http_transfer.hrl").

start(_Type, _Args) ->
	web_start(),
	http_transfer_sup:start_link().

stop(_State) ->
	ok.

web_start() ->
	WebConfig = application:get_env(?APPNAME, web_config, []),
	HttpPort = proplists:get_value(http_port, WebConfig, 8080),
	PoolSize = proplists:get_value(pool_size, WebConfig, 10),

	{ok, _} = cowboy:start_http(?COWBOY_REF, PoolSize, [
		{port, HttpPort},
		{max_connections, 100}],
		[{env, [{dispatch, dispatch()}]}
		]).

dispatch() ->
	cowboy_router:compile([
		{'_', [
			{"/[...]", handler_default, []}
		]}
	]).