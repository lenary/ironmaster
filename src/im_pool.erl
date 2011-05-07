% The contents of this file are subject to the Erlang Public License,
% Version 1.1, (the "License"); you may not use this file except in
% compliance with the License. You should have received a copy of the
% Erlang Public License along with this software. If not, it can be
% retrieved via the world wide web at http://www.erlang.org/.

% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
% the License for the specific language governing rights and limitations
% under the License.

% The Initial Developer of the Original Code is Samuel Elliott.
% Portions created by Samuel Elliott are Copyright 2011, Samuel Elliott.
% All Rights Reserved.

-module(im_pool).

-behaviour(gen_fsm).

% FSM:
%  [idling]
%      |
%      v (start)
% [preparing] -> [finished]
%     | ^ (node_finished)
%   * v |
% [converging]


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/1,
         begin_operation/1,
         % finished_operation/1,
         add_servers/2
        ]).

-export([behaviour_info/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

-export([
         idling/2,
         preparing/2,
         converging/2,
         finished/2
        ]).

-record(pool, {
          type,
          state
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

behaviour_info(callbacks) ->
  [
    {init,1},
    {handle_add,2},
    {handle_start,1},
    {handle_next,1},
    {handle_prepare,1},
    {handle_converge,1},
    {handle_finish,1},
    {handle_info,2},
    {terminate,2},
    {code_change,3}
  ];
behaviour_info(_) ->
  undefined.

% TODO: we can extract the identifier better than this,
% probably at the supervisor level
start_link(Args) ->
  Id = im_utils:identifier(proplists:get_value(pool_type, Args)),
  Args1 = [{identifier, Id}| Args],
  gen_fsm:start_link({local, Id}, ?MODULE, Args1, []).

begin_operation(Id) ->
  gen_fsm:send_event(Id, start).

% finished_operation(Operation) ->
%   gen_fsm:send_event(im_utils:pool_name(Operation), node_finished).

add_servers(Id, Nodes) when is_list(Nodes) ->
  gen_fsm:send_event(Id, {add_servers, Nodes});
add_servers(Id, Node) ->
  add_servers(Id, [Node]).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  PoolType = proplists:get_value(pool_type, Args),
  Args1 = proplists:delete(pool_type, Args),
  {ok, PoolState} = PoolType:init(Args1),
  {ok, idling, #pool{type=PoolType, state = PoolState}}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(Info, StateName, State) ->
  {ok, PoolState} = (State#pool.type):handle_info(Info,
                                                  State#pool.state),
  {next_state, StateName, merge_pool_state(State, PoolState)}.

terminate(Reason, _StateName, State) ->
  (State#pool.type):terminate(Reason, State#pool.state).

code_change(OldVsn, StateName, State, Extra) ->
  {ok, PoolState} = (State#pool.type):code_change(OldVsn,
                                                  State#pool.state,
                                                  Extra),
  {ok, StateName, merge_pool_state(State, PoolState)}.

%% ------------------------------------------------------------------
%% State Function Definitions
%% ------------------------------------------------------------------

idling({add_servers, Servers}, State) ->
  {ok, PoolState} = (State#pool.type):handle_add(Servers, State#pool.state),
  {next_state, idling, merge_pool_state(State, PoolState)};

idling(start, State) ->
  State1 = case call_pool_fun(handle_start, State) of
    {ok, PoolState} -> preparing;
    {error, _}      -> PoolState = State#pool.state,
                       idling
  end,
  {next_state, State1, merge_pool_state(State, PoolState), 0};

idling(_Event, State) ->
  {next_state, idling, State}.


preparing(timeout, State) ->
  State1 = case call_pool_fun(handle_next, State) of
    {ok, PoolState}   -> ok = (State#pool.type):handle_prepare(PoolState),
                         converging;
    {done, PoolState} -> finished
  end,
  {next_state, State1, merge_pool_state(State, PoolState), 0};

preparing(_Event, State) ->
  {next_state, preparing, State}.


converging(timeout, State) ->
  ok = call_pool_fun(handle_converge, State),
  {next_state, converging, State};

converging(node_finished, State) ->
  % callback here?
  {next_state, preparing, State, 0};

converging(_Event, State) ->
  {next_state, converging, State}.


finished(timeout, State) ->
  ok = call_pool_fun(handle_finish, State),
  {next_state, finished, State};

finished(_Event, State) ->
  {next_state, finished, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

merge_pool_state(State, PoolState) ->
  State#pool{state=PoolState}.

call_pool_fun(Fun, State) ->
  (State#pool.type):Fun(State#pool.state).
