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

-module(im_operation_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/1
        ]).

%% Supervisor callbacks
-export([
         init/1
        ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child([Operation, _Node, Pool] = Opts) ->
  Name = im_utils:operation_name(Pool),
  supervisor:start_child(?MODULE, {Name, {im_operation, start_link, Opts}, transient, 5000, worker, [Operation]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
