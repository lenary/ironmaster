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
         start/3,
         start_link/1
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
         finished/2
        ]).

-record(operation_info, {
          operation = undefined,
          node
        }).

%% ===================================================================
%% API functions
%% ===================================================================

behaviour_info(callbacks) ->
  [
   {handle_check, 1},
   {handle_converge, 1}
  ];
behaviour_info(_Other) ->
  undefined.

start(Operation, Node, Extra) ->
  im_operation_sup:start_child(Operation, Node, Extra).

% TODO
start_link(Args) ->
  gen_fsm:start_link({local, name}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Operation = proplists:get_value(operation, Args),
  Node      = proplists:get_value(node, Args),
  if Operation =:= undefined -> {stop, no_operation};
     Operation =/= undefined ->
        {ok, checking, #operation_info{node=Node, operation=Operation}, 0}
  end.

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

before_checking(timeout, #operation_info{operation=Operation} = State) ->
  NextState = case Operation:handle_check(State) of
                true  -> finished;
                false -> converging
              end,
  {next_state, NextState, State, 0};
before_checking(_Event, State) ->
  {next_state, before_checking, State}.

converging(timeout, #operation_info{operation=Operation} = State) ->
  Operation:handle_converge(State),
  {next_state, after_checking, State, 0};
converging(_Event, State) ->
  {next_state, converging, State}.

after_checking(timeout, #operation_info{operation=Operation} = State) ->
  NextState = case Operation:handle_check(State) of
                true  -> finished;
                false -> error
              end,
  {next_state, NextState, State, 0};
after_checking(_Event, State) ->
  {next_state, after_checking, State}.

finished(timeout, State) ->
  % notify
  {stop, normal, State};
finished(_Event, State) ->
  {next_state, finished, State}.

error(timeout, State) ->
  % notify
  {stop, error, State};
error(_Event, State) ->
  {next_state, error, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------