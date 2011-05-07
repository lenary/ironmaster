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

-module(ironmaster).

-author('Sam Elliott <sam@lenary.co.uk>').

-export([
         new_operation/3
        ]).

% TODO: verify at the start_link level.
new_operation(Operation, PoolType, Servers) ->
  true = im_utils:verify_is(im_operation, Operation) and
         im_utils:verify_is(im_pool, PoolType),
  im_pool_sup:start_link(Operation, PoolType, Servers).

