%%
%% {{appid}}_server.erl
%% {{appid}} server
%%
-module({{appid}}_server).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-record(gen_server_data, {
  id1   :: non_neg_integer(),
  id2   :: non_neg_integer(),
  id3   :: non_neg_integer()
}).

%%====================================================================
%% API functions
%%====================================================================
-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).

%%%===================================================================
%%% INTERFACE API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link([Id1,Id2,Id3]) ->
  ?LOG_INFO("Gen-Server Start Link: ~p.~p.~p", [Id1,Id2,Id3]),
  GenServerName = {{appid}}_sup:compose_gen_server_name(Id1,Id2,Id3),
  gen_server:start_link({local, GenServerName}, ?MODULE, [Id1,Id2,Id3], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Id1,Id2,Id3]) ->
  logger:set_module_level(?MODULE,all),
  ?LOG_INFO("Gen-Server Init: ~p.~p.~p", [Id1,Id2,Id3]),
  GenServerData = #gen_server_data { id1 = Id1,
                                     id2 = Id2,
                                     id3 = Id3 },
% comment this line to stop trapping exits
  process_flag(trap_exit, true), % comment this line to stop trapping exits
  {ok, GenServerData}.

handle_cast(Msg, State) ->
  ?LOG_INFO(#{handle => unknown_cast, msg => Msg}),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  ?LOG_INFO(#{handle => unknown_call, msg => Msg}),
  {noreply, State}.

handle_info(Info, State) ->
  ?LOG_INFO(#{handle => unknown_info, msg => Info}),
  {noreply, State}.

%% @private
terminate(_Reason, State) ->
  ?LOG_INFO("Gen-Server Terminate: ~p.~p.~p", [State#gen_server_data.id1,
                                               State#gen_server_data.id2,
                                               State#gen_server_data.id3]),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
