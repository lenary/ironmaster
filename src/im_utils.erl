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

-module(im_utils).

-export([
         operation_name/1,
         pool_name/1
        ]).

-export([
         verify_is/2
        ]).

pool_name(Operation) ->
  identifier_to_atom(Operation, pool).

operation_name(Operation) ->
  identifier_to_atom(Operation, operation).

identifier_to_atom(Identifier, Type) when is_atom(Type) ->
  identifier_to_atom(Identifier, atom_to_list(Type));

identifier_to_atom(Identifier, Type) ->
  Identifier1 = atom_to_list(Identifier) ++ "_" ++  Type,
  list_to_atom(Identifier1).


verify_is(Behaviour, Module) ->
  code:is_loaded(Module) and
  lists:all(fun (T) -> T end,
            [ erlang:function_exported(Module, Fun, Arity) ||
              {Fun, Arity} <- Behaviour:behaviour_info(callbacks)]).
