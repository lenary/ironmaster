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

-module(im_audit_log).

-export ([
          start_link/0,
          add_handler/2,
          delete_handler/2,
          notify/1
         ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?MODULE, Handler, Args).

notify(Event) ->
  gen_event:notify(?MODULE, Event).
