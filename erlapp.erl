%%
%% {{appid}}.erl
%% {{appid}} entry point
%%
-module({{appid}}).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    {{appid}}_sup:start_link().

start() ->
    application:start({{appid}}),
    application:ensure_all_started(folsom),
    application:ensure_all_started(gproc).

stop() ->
    application:stop(gproc),
    application:stop(folsom),
    application:stop({{appid}}).
