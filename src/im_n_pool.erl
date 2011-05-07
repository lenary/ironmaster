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

-behaviour(im_pool).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/1
        ]).

%% ------------------------------------------------------------------
%% im_pool Function Exports
%% ------------------------------------------------------------------

-export([
         init/1,
         handle_add/2,
         handle_start/1,
         handle_next/1,
         handle_prepare/1,
         handle_converge/1,
         handle_finish/1,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(n_pool, {
          todo = [],  % servers yet to be checked
          done = [],  % servers already checked
          current,    % server currently being checked
          operation,  % the module containing the operation
          identifier  % auto-generated fsm instance identifier
        }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
  im_pool:start_link([{pool_type, ?MODULE}|Args]).

%% ------------------------------------------------------------------
%% im_pool Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Todo      = proplists:get_value(todo,       Args, []),
  Operation = proplists:get_value(operation,  Args),
  Id        = proplists:get_value(identifier, Args),
  {ok, #n_pool{todo=Todo, operation=Operation, identifier=Id}}.

handle_add(Servers, #n_pool{todo=Todo} = State) ->
  State1 = State#n_pool{todo = Todo ++ Servers},
  {ok, State1}.

handle_start(State) ->
  case State#n_pool.todo of
    []                        -> {error, no_nodes};
    Todos when is_list(Todos) -> {ok, next_node(State)}
  end.

handle_next(State) ->
  NewState = next_node(State),
  {ok, NewState}.

handle_prepare(_State) ->
  ok.

% TODO
handle_converge(_State) ->
  % spawn im_operation here, passing through identifier.
  % Use this identifier any time a callback is sent through
  ok.

handle_finish(_State) ->
  ok.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

next_node(#n_pool{todo=[First|Todo], current=undefined} = ServerPool) ->
  ServerPool#n_pool{todo=Todo, current=First};

next_node(#n_pool{todo=[Next|Todo], current=Previous, done=Done} = ServerPool) ->
  ServerPool#n_pool{todo=Todo, current=Next, done=[Previous|Done]}.
