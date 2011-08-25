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

-module(im_audit_lager).

-behaviour(gen_event).

% magic for lager to work
-compile([{parse_transform, lager_transform}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         add_handler/0
        ]).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ===================================================================
%% API functions
%% ===================================================================

add_handler() ->
  im_audit_log:add_handler(?MODULE, []).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, state}.

handle_event({pool_started, PoolName, Nodes}, State) ->
  lager:notice("Pool Started: ~p with Nodes: ~p", [PoolName, Nodes]),
  {ok, State};
handle_event({pool_spare_up, PoolName, Spare}, State) ->
  lager:info("Pool: ~p Started Spare: ~p", [PoolName, Spare]),
  {ok, State};
handle_event({pool_started_node, PoolName, Operation, Node}, State) ->
  lager:debug("Pool: ~p Started Operation: ~p on Node: ~p", [PoolName, Operation, Node]),
  {ok, State};
handle_event({pool_finished_node, PoolName, Operation, Node}, State) ->
  lager:debug("Pool: ~p Finished Operation: ~p on Node: ~p", [PoolName, Operation, Node]),
  {ok, State};
handle_event({pool_spare_down, PoolName, Spare}, State) ->
  lager:info("Pool: ~p Shutdown Spare: ~p", [PoolName, Spare]),
  {ok, State};
handle_event({pool_finished, PoolName, Nodes}, State) ->
  lager:notice("Pool Finished: ~p with Nodes: ~p", [PoolName, Nodes]),
  {ok, State};
handle_event({operation_converge_failed, Operation, Node}, State) ->
  lager:error("Converge Failed for Operation: ~p on Node: ~p", [Operation, Node]),
  {ok, State};
handle_event({ssh_connected, Node}, State) ->
  lager:notice("SSH Connected Node: ~p", [Node]),
  {ok, State};
handle_event({ssh_started, Node, Command}, State) ->
  lager:debug("SSH Started Command: '~p' on Node: ~p", [Command, Node]),
  {ok, State};
handle_event({ssh_finished, Node, Command}, State) ->
  lager:debug("SSH Finished Command: '~p' on Node: ~p", [Command, Node]),
  {ok, State};
handle_event({ssh_disconnected, Node}, State) ->
  lager:notice("SSH Disconnected Node: ~p", [Node]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

