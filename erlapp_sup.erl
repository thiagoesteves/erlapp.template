%%
%% {{appid}}_sup.erl
%% {{appid}} supervisor
%%
-module({{appid}}_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_gen_server/3, delete_gen_server/3,
         compose_gen_server_name/3]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(GEN_SERVER_TIMEOUT, 5000). % ms

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    logger:set_module_level(?MODULE,info),
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    % The supervisor starts out empty...
    {ok, {SupFlags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_gen_server(Id1,Id2,Id3) ->
  ?LOG_INFO("Creating Gen-Server: ~p.~p.~p", [Id1,Id2,Id3]),
  GenServerName = compose_gen_server_name(Id1,Id2,Id3),
  GenServerSpec = {GenServerName, { {{appid}}_server, start_link, [[Id1,Id2,Id3]]},
        permanent, ?GEN_SERVER_TIMEOUT, worker, [{{appid}}_server]},
  supervisor:start_child({{appid}}_sup, GenServerSpec).

delete_gen_server(Id1,Id2,Id3) ->
  ?LOG_INFO("Deleting Gen-Server: ~p.~p.~p", [Id1,Id2,Id3]),
  GenServerName = compose_gen_server_name(Id1,Id2,Id3),
  supervisor:terminate_child({{appid}}_sup, GenServerName).

%%====================================================================
%% Internal functions
%%====================================================================

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).

%% compose gen server name id1.id2.id3
compose_gen_server_name(Id1,Id2,Id3) ->
  Name = "Gen:"++i2l(Id1)++"."++i2l(Id2)++"."++i2l(Id3),
  l2a(Name).
