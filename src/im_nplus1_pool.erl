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

-module(im_nplus1_pool).

% FSM:
%  [idling]
%      |
%      v (finished_spare)
% [preparing] -> [finished]
%     | ^ (finished_node)
%   * v |
% [converging]

-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/5,
         start_operation/1,
         finished_node/2,
         finished_spare/2,
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

-record(nplus1_pool, {
          todo = [],  % servers yet to be checked
          done = [],  % servers already checked
          current,    % server currently being checked
          spare,      % server currently spare
          operation,  % the module containing the operation
          provider,   % the module containing info to bring the new server up
          name        %
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Operation, Provider, Spare, Todo) ->
  true = im_utils:verify_is(im_operation, Operation),
  true = im_utils:verify_is(im_provider, Provider),
  Name1 = im_utils:pool_name(Name),
  gen_fsm:start_link({local, Name1}, ?MODULE, [Name, Operation, Provider, Spare, Todo], []).

start_operation(Name) ->
  im_pool:start_operation(Name).

finished_node(Name, Node) ->
  im_pool:finished_node(Name, Node).

finished_spare(Name, Node) ->
  im_pool:finished_spare(Name, Node).

add_nodes(Name, Nodes)->
  im_poool:add_nodes(Name, Nodes).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Name, Operation, Provider, Spare, Todo]) ->
  State = #nplus1_pool{name=Name, operation=Operation, provider=Provider, spare=Spare, todo=Todo},
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
  im_audit_log:notify({pool_started, State#nplus1_pool.name, State#nplus1_pool.todo}),
  ok = setup_spare(State),
  {next_state, idling, State};

idling({finished_spare, Spare}, #nplus1_pool{spare=Spare} = State) ->
  im_audit_log:notify({pool_spare_up, State#nplus1_pool.name, State#nplus1_pool.spare}),
  {next_state, preparing, State, 0};

idling(_Event, State) ->
  {next_state, idling, State}.

idling({add_nodes, Nodes}, _From, #nplus1_pool{todo=Todo} = State) ->
  State1 = State#nplus1_pool{todo = Todo ++ Nodes},
  {reply, {ok, State1#nplus1_pool.todo}, idling, State1};

idling(_Event, _From, State) ->
  {reply, ok, idling, State}.


preparing(timeout, PoolState) ->
  {NewState, PoolState1} = choose(PoolState),
  ok = hotswap(PoolState1),
  {next_state, NewState, PoolState1, 0};

preparing(_Event, State) ->
  {next_state, preparing, State}.

preparing({add_nodes, _}, _From, State) ->
  {reply, not_allowed, preparing, State};

preparing(_Event, _From, State) ->
  {reply, ok, preparing, State}.


converging(timeout, State) ->
  im_audit_log:notify({pool_started_node, State#nplus1_pool.name, State#nplus1_pool.current}),
  % TODO: start converging here.
  {next_state, converging, State};

converging({finished_node, Node}, #nplus1_pool{current=Node} = State) ->
  im_audit_log:notify({pool_finished_node, State#nplus1_pool.name, State#nplus1_pool.current}),
  {next_state, preparing, State, 0};

converging(_Event, State) ->
  {next_state, converging, State}.

converging({add_nodes, _}, _From, State) ->
  {reply, not_allowed, converging, State};

converging(_Event, _From, State) ->
  {reply, ok, converging, State}.


finished(timeout, State) ->
  im_audit_log:notify({pool_spare_down, State#nplus1_pool.name, State#nplus1_pool.spare}),
  ok = teardown_spare(State),
  im_audit_log:notify({pool_finished, State#nplus1_pool.name, State#nplus1_pool.done}),
  {stop, normal, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

choose(#nplus1_pool{done=Done, spare=Spare, current=Current,   todo=[]} = ServerPool) ->
  ServerPool1 = ServerPool#nplus1_pool{done=[Spare|Done], spare=Current, current=undefined},
  {finished, ServerPool1};

choose(#nplus1_pool{done=[],   spare=_Spare, current=undefined, todo=[Next|Todo]} = ServerPool) ->
  ServerPool1 = ServerPool#nplus1_pool{current=Next, todo=Todo},
  {converging, ServerPool1};

choose(#nplus1_pool{done=Done, spare=Spare, current=Current,   todo=[Next|Todo]} = ServerPool) ->
  ServerPool1 = ServerPool#nplus1_pool{done=[Spare|Done], spare=Current, current=Next, todo=Todo},
  {converging, ServerPool1}.


  % TODO: set off something here to hotswap stuff
hotswap(_ServerPool) ->
  ok.

  % TODO: set off something here to prepare the spare
setup_spare(_ServerPool) ->
  ok.

  % TODO: set off something here to teardown the spare
teardown_spare(_ServerPool) ->
  ok.
