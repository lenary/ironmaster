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

-export([
         start_operation/1,
         finished_node/2,
         finished_spare/2,
         add_nodes/2
        ]).

start_operation(Name) ->
  Name1 = im_utils:pool_name(Name),
  gen_fsm:send_event(Name1, start_operation).


finished_node(Name, Node) ->
  Name1 = im_utils:pool_name(Name),
  gen_fsm:send_event(Name1, {finished_node, Node}).


finished_spare(Name, Node) ->
  Name1 = im_utils:pool_name(Name),
  gen_fsm:send_event(Name1, {finished_spare, Node}).


add_nodes(Name, Node) when is_atom(Node) ->
  add_nodes(Name, [Node]);

add_nodes(Name, Nodes) when is_list(Nodes) ->
  Name1 = im_utils:pool_name(Name),
  gen_fsm:send_sync_event(Name1, {add_nodes, Nodes}).
