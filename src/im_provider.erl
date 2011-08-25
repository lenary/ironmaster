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

-module(im_provider).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/1,
         behaviour_info/1,
         bootstrap/2,
         shutdown/2
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(provider, {
          provider % the module containing the callbacks
        }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Provider) ->
  Name = im_utils:provider_name(Provider),
  case gen_server:start_link({local, Name}, ?MODULE, [Provider], []) of
    ok                            -> ok;
    {error, {already_started, _}} -> ok;
    Else                          -> Else
  end.

behaviour_info(callbacks) ->
  [
   {bootstrap_server, 2},
   {shutdown_server, 2},
   {hotswap_server, 3}
  ];
behaviour_info(_Other) ->
  undefined.

bootstrap(Provider, Node) ->
  Name = im_utils:provider_name(Provider),
  gen_server:call(Name, {bootstrap_server, Node}, infinity).

shutdown(Provider, Node) ->
  Name = im_utils:provider_name(Provider),
  gen_server:call(Name, {shutdown_server, Node}, infinity).

hotswap(Provider, CurrNode, SpareNode) ->
  Name = im_utils:provider_name(Provider),
  gen_server:call(Name, {hotswap_server, CurrNode, SpareNode}, infinity).

finished(ReplyTo, Result) ->
  gen_server:reply(ReplyTo, Result),
  ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Provider]) ->
  State = #provider{provider=Provider},
  {ok, State}.

handle_call({bootstrap_server, Node}, From, State) ->
  bootstrap_server(State, Node, From),
  {noreply, State};

handle_call({shutdown_server, Node}, From, State) ->
  shutdown_server(State, Node, From),
  {noreply, State};

handle_call({hotswap_server, CurrNode, SpareNode}, From, State) ->
  hotswap_server(State, CurrNode, SpareNode, From),
  {noreply, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

bootstrap_server(State, Node, ReplyTo) ->
  (State#provider.provider):bootstrap_server(Node, ReplyTo).

shutdown_server(State, Node, ReplyTo) ->
  (State#provider.provider):shutdown_server(Node, ReplyTo).

hotswap_server(State, CurrNode, SpareNode, ReplyTo) ->
  (State#provider.provider):hotswap_server(CurrNode, SpareNode, ReplyTo).
