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

-module(im_operation).

-behaviour(gen_fsm).

% sub-behaviour of gen_fsm

% this is a behaviour for our slaves, so that lots of behaviour can
% be abstracted

% FSM:
% [before_checking] -> [finished]
%        |
%        v
%   [converging]
%        |
%        v
% [after_checking] -> [finished]
%        |
%        v
%     [error]

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         behaviour_info/1,
         start_link/3,
         finished/2
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
         before_checking/2,
         converging/2,
         after_checking/2,
         error/2,
         done/2
        ]).

-record(operation, {
          operation,
          pool,
          node
        }).

%% ===================================================================
%% API functions
%% ===================================================================

behaviour_info(callbacks) ->
  [
   {handle_check, 2},
   {handle_converge, 2}
  ];
behaviour_info(_Other) ->
  undefined.

start_link(Operation, Node, Pool) ->
  true = im_utils:verify_is(im_operation, Operation),
  Name = im_utils:operation_name(Pool),
  gen_fsm:start_link({local, Name}, ?MODULE, [Operation, Node, Pool], []).

finished(Pool, Result) ->
  Name = im_utils:operation_name(Pool),
  gen_fsm:send_event(Name, {finished, Result}),
  ok.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Operation, Node, Pool]) ->
  {ok, before_checking, #operation{operation=Operation, node=Node, pool=Pool}, 1}.

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
%% State Function Definitions
%% ------------------------------------------------------------------

before_checking(timeout, State) ->
  handle_check(State),
  {next_state, before_checking, State};

before_checking({finished, Result}, State) ->
  NextState = case Result of
                true  -> finished;
                false -> converging
              end,
  {next_state, NextState, State, 1};

before_checking(_Event, State) ->
  {next_state, before_checking, State}.


converging(timeout, State) ->
  handle_converge(State),
  {next_state, converging, State};

converging({finished, _Result}, State) ->
  {next_state, after_checking, State, 1};

converging(_Event, State) ->
  {next_state, converging, State}.


after_checking(timeout, State) ->
  handle_check(State),
  {next_state, after_checking, State};

after_checking({finished, Result}, State) ->
  NextState = case Result of
                true  -> done;
                false -> error
              end,
  {next_state, NextState, State, 1};

after_checking(_Event, State) ->
  {next_state, after_checking, State}.


done(timeout, State) ->
  im_pool:finished_node(State#operation.pool, State#operation.node),
  {stop, normal, State}.


error(timeout, State) ->
  im_audit_log:notify({
                       operation_converge_failed,
                       State#operation.operation,
                       State#operation.node
                      }),
  im_pool:finished_node(State#operation.pool, State#operation.node),
  {stop, normal, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

handle_check(#operation{operation=Operation, node=Node, pool=Pool}) ->
  Operation:handle_check(Pool, Node).

handle_converge(#operation{operation=Operation, node=Node, pool=Pool}) ->
  Operation:handle_converge(Pool, Node).
