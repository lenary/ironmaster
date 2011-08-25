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

-module(im_n_pool).

% FSM:
%  [idling]
%      |
%      v (start)
% [preparing] -> [finished]
%     | ^ (node_finished)
%   * v |
% [converging]

-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/3,
         start_operation/1,
         finished_node/2,
         add_nodes/2
        ]).

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
         idling/3,
         preparing/2,
         preparing/3,
         converging/2,
         converging/3,
         finished/2
        ]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(n_pool, {
          todo = [],  % servers yet to be checked
          done = [],  % servers already checked
          current,    % server currently being checked
          operation,  % the module containing the operation
          name        %
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Operation, Todo) ->
  true = im_utils:verify_is(im_operation, Operation),
  Name1 = im_utils:pool_name(Name),
  gen_fsm:start_link({local, Name1}, ?MODULE, [Name, Operation, Todo], []).

start_operation(Name) ->
  im_pool:start_operation(Name).

finished_node(Name, Node) ->
  im_pool:finished_node(Name, Node).

add_nodes(Name, Nodes)->
  im_poool:add_nodes(Name, Nodes).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Name, Operation, Todo]) ->
  State = #n_pool{name=Name, operation=Operation, todo=Todo},
  {ok, idling, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% gen_fsm State Callback Definitions
%% ------------------------------------------------------------------

idling(start_operation, State) ->
  im_audit_log:notify({pool_started, State#n_pool.name, State#n_pool.todo}),
  {next_state, preparing, State, 1};
idling(_Event, State) ->
  {next_state, idling, State}.

idling({add_nodes, Nodes}, _From, #n_pool{todo=Todo} = State) ->
  State1 = State#n_pool{todo = Todo ++ Nodes},
  {reply, {ok, State#n_pool.todo}, idling, State1};

idling(_Event, _From, State) ->
  {reply, ok, idling, State}.



preparing(timeout, PoolState) ->
  {NewState, PoolState1} = choose(PoolState),
  {next_state, NewState, PoolState1, 1};

preparing(_Event, State) ->
  {next_state, preparing, State}.

preparing({add_nodes, _}, _From, State) ->
  {reply, not_allowed, preparing, State};

preparing(_Event, _From, State) ->
  {reply, ok, preparing, State}.


converging(timeout, State) ->
  im_audit_log:notify({pool_started_node, State#n_pool.name, State#n_pool.current}),
  start_converging(State),
  {next_state, converging, State};

converging(finished_node, State) ->
  im_audit_log:notify({pool_finished_node, State#n_pool.name, State#n_pool.operation, State#n_pool.current}),
  {next_state, preparing, State, 1};

converging(_Event, State) ->
  {next_state, converging, State}.

converging({add_nodes, _}, _From, State) ->
  {reply, not_allowed, converging, State};

converging(_Event, _From, State) ->
  {reply, ok, converging, State}.


finished(timeout, State) ->
  im_audit_log:notify({pool_finished, State#n_pool.name, State#n_pool.operation, State#n_pool.done}),
  {stop, normal, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

choose(#n_pool{done=Done, current=Current, todo=[]} = ServerPool) ->
  ServerPool1 = ServerPool#n_pool{done=[Current|Done], current=undefined},
  {finished, ServerPool1};

choose(#n_pool{done=[], current=undefined, todo=[Next|Todo]} = ServerPool) ->
  ServerPool1 = ServerPool#n_pool{current=Next, todo=Todo},
  {converging, ServerPool1};

choose(#n_pool{done=Done, current=Current, todo=[Next|Todo]} = ServerPool) ->
  ServerPool1 = ServerPool#n_pool{done=[Current|Done], current=Next, todo=Todo},
  {converging, ServerPool1}.


start_converging(#n_pool{operation=Operation, current=Node, name=Pool}) ->
  im_operation_sup:start_child([Operation, Node, Pool]).
