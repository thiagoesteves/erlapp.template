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
  id3   :: non_neg_integer(),
  temperature :: atom()
}).

%%====================================================================
%% API functions
%%====================================================================
-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([compose_gen_server_name/3, collectData/3]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(NUMBER_OF_VALUES, 5).

%%%===================================================================
%%% INTERFACE API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link([Id1,Id2,Id3]) ->
  ?LOG_INFO("Gen-Server Start Link: ~p.~p.~p", [Id1,Id2,Id3]),
  GenServerName = compose_gen_server_name(Id1,Id2,Id3),
  gen_server:start_link({local, GenServerName}, ?MODULE, [Id1,Id2,Id3], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Id1,Id2,Id3]) ->
  logger:set_module_level(?MODULE,error),
  ?LOG_INFO("Gen-Server Init: ~p.~p.~p", [Id1,Id2,Id3]),
  % Initialise metric to be read
  TempName = compose_temperature_name(Id1,Id2,Id3),
  folsom_metrics:new_history(TempName, ?NUMBER_OF_VALUES),
  % Register Gproc for the Interface
  GprocState = gproc:reg(gen_server_id(Id1,Id2,Id3),gproc),
  ?LOG_INFO(#{ gproc => {GprocState} }),
  % Save data to state
  GenServerData = #gen_server_data { id1 = Id1,
                                     id2 = Id2,
                                     id3 = Id3,
                                     temperature = TempName},
  % comment this line to stop trapping exits
  process_flag(trap_exit, true), % comment this line to stop trapping exits
  {ok, GenServerData}.

handle_cast(Msg, State) ->
  ?LOG_INFO(#{handle => unknown_cast, msg => Msg}),
  {noreply, State}.

handle_call({collectData}, _From, State) ->
  ?LOG_INFO("Collecting Data for Gen-Server: ~p", [State#gen_server_data.temperature]),
  Res = collectData(State#gen_server_data.temperature),
  {reply, Res, State};

handle_call(Msg, _From, State) ->
  ?LOG_INFO(#{handle => unknown_call, msg => Msg}),
  {noreply, State}.

handle_info(Info, State) ->
  ?LOG_INFO(#{handle => unknown_info, msg => Info}),
  {noreply, State}.

%% @private
terminate(Reason, State) ->
  ?LOG_INFO("Gen-Server Terminate: ~p.~p.~p", [State#gen_server_data.id1,
                                               State#gen_server_data.id2,
                                               State#gen_server_data.id3]),
  GprocState = gproc:goodbye(),
  ?LOG_INFO(#{
      reason            => Reason,
      state             => State,
      gproc_state       => GprocState
  }),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Server functions
%%====================================================================
collectData(Id) ->
  % Read data here
  TempValue = rand:uniform(100),
  folsom_metrics:notify({Id, TempValue}),
  ok.

%%====================================================================
%% External functions
%%====================================================================

collectData(Id1,Id2,Id3) ->
    Pids = gproc:lookup_pids(gen_server_id(Id1,Id2,Id3)),
    Retval = case Pids of
      [] ->
        ?LOG_WARNING(#{
          server_id => {Id1,Id2,Id3}, pid => "no pid"
        }),
        {error,no_pid};
      [Pid] -> gen_server:call(Pid, {collectData})
    end,
    Retval.

%%====================================================================
%% Internal functions
%%====================================================================

% Create Gproc info for register function
gen_server_id(Id1,Id2,Id3) ->
  GProcKey = compose_gen_server_name(Id1,Id2,Id3),
  {p,l,{?MODULE,GProcKey}}.

%% compose gen server name Gen:id1.id2.id3
compose_gen_server_name(Id1,Id2,Id3) ->
  Name = "Gen:"++i2l(Id1)++"."++i2l(Id2)++"."++i2l(Id3),
  l2a(Name).

%% compose gen server name Gen:id1.id2.id3
compose_temperature_name(Id1,Id2,Id3) ->
  Name = "temp:"++i2l(Id1)++"."++i2l(Id2)++"."++i2l(Id3),
  l2a(Name).

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).
