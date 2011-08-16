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

-module(im_ssh).

-behaviour(gen_server).

-define(SSH_TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/1,
         exec/3
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

-record(ssh, {
          node,           % the connected node
          connection,     % the internal ssh connection ref
          channels = [],  % a proplist of (channel ref -> {cmd, fn}) pairs
          buffers = []    % a proplist of (channel ref -> {stdout, stderr}) pairs
        }).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Node) ->
  Name = im_utils:ssh_name(Node),
  gen_server:start_link({local, Name}, ?MODULE, [Node], []).


exec(Node, Command, Callback) ->
  Name = im_utils:ssh_name(Node),
  gen_server:cast(Name, {exec, Command, Callback}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Node]) ->
  % TODO: get node connection info here
  {node_info, Host, Port, User} = im_utils:mock_data(Node, ssh),
  case ssh:connect(Host, Port, [
                                {silently_accept_hosts, false},
                                {user_interaction, false},
                                {user, User},
                                {connect_timeout, ?SSH_TIMEOUT}
                               ]) of
    {ok, Connection} -> State = connected(Node, Connection),
                        {ok, State};
    Error            -> {stop, Error}
  end.


handle_call({exec, Command, CallbackFn}, _From, #ssh{connection=Connection} = State) ->
  im_audit_log:notify({ssh_started, State#ssh.node, Command}),
  {ok, Channel} = ssh_connection:session_channel(Connection, ?SSH_TIMEOUT),
  case ssh_connection:exec(Connection, Channel, Command, ?SSH_TIMEOUT) of
    success ->  NewState = executed(State, Channel, Command, CallbackFn),
                {reply, ok, NewState};
    Error   ->  {reply, Error, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info({ssh_cm, _ConnRef, {data, ChannelId, DataType, Data}}, #ssh{buffers=Buffers} = State) ->
  case proplists:get_value(Buffers, ChannelId) of
    {StdOut, StdErr} -> NewState = data_received(State, ChannelId, StdOut, StdErr, DataType, Data),
                        {noreply, NewState};
    undefined        -> {noreply, State}
  end;

handle_info({ssh_cm, _ConnRef, {eof, ChannelId}}, #ssh{channels=Channels} = State) ->
  case proplists:get_value(Channels, ChannelId) of
    {Command, CallbackFn} -> NewState = eof_received(State, ChannelId, Command, CallbackFn),
                             {noreply, NewState};
    undefined             -> {noreply, State}
  end;

handle_info({ssh_cm, _ConnRef, {closed, ChannelId}}, #ssh{channels=Channels} = State) ->
  case proplists:get_value(Channels, ChannelId) of
    {Command, _CallbackFn} -> NewState = closed(State, ChannelId, Command),
                              {noreply, NewState};
    undefined              -> {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, #ssh{connection=Connection, node=Node} = _State) ->
  im_audit_log:notify({ssh_disconnected, Node}),
  ok = ssh:close(Connection),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec concat_data(binary(), binary(), 0|1, binary()) -> {binary(), binary()}.
concat_data(StdOut, StdErr, 0, Data) ->
  StdOut1 = list_to_binary([StdOut, Data]),
  {StdOut1, StdErr};

concat_data(StdOut, StdErr, 1, Data) ->
  StdErr1 = list_to_binary([StdErr, Data]),
  {StdOut, StdErr1};

concat_data(StdOut, StdErr, _DataType, _Data) ->
  {StdOut, StdErr}.


connected(Node, Connection) ->
  im_audit_log:notify({ssh_connected, Node}),
  #ssh{node=Node, connection=Connection}.


executed(State, Channel, Command, CallbackFn) ->
  Channels  = [{Channel, {Command, CallbackFn}}|State#ssh.channels],
  Buffers   = [{Channel, {<<>>, <<>>}}|State#ssh.buffers],
  State#ssh{channels=Channels, buffers=Buffers}.


data_received(State, ChannelId, StdOut, StdErr, DataType, Data) ->
  {StdOut1, StdErr1} = concat_data(StdOut, StdErr, DataType, Data),
  Buffers1 = [{ChannelId, {StdOut1, StdErr1}}|proplists:delete(State#ssh.buffers,ChannelId)],
  State#ssh{buffers=Buffers1}.


eof_received(State, ChannelId, Command, CallbackFn) ->
  im_audit_log:notify({ssh_eof, State#ssh.node, Command}),
  {StdOut, StdErr} = proplists:get_value(State#ssh.buffers, ChannelId),
  CallbackFn(StdOut, StdErr),
  State.


closed(State, ChannelId, Command) ->
  im_audit_log:notify({ssh_finished, State#ssh.node, Command}),
  Channels1 = proplists:delete(State#ssh.channels, ChannelId),
  Buffers1 = proplists:delete(State#ssh.buffers, ChannelId),
  State#ssh{channels=Channels1, buffers=Buffers1}.

