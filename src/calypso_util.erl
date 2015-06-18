-module(calypso_util).
-author("begemot").

%% API
-export([
  maps_binary_key_to_atom/1,
  validate/2,
  definition/1, ensure_function_exported/1,
  to_integer/1, to_boolean/1
]).

maps_binary_key_to_atom(Data) when is_map(Data) ->
  Result = maps:fold(fun
    (Key,Value, Acc) when is_binary(Key) ->
      [ { binary_to_existing_atom(Key, utf8), Value } | Acc ];
    (Key, Value, Acc) when is_atom(Key) ->
      [ {Key, Value } | Acc ]
  end, [], Data),
  maps:from_list(Result).

validate(Data, Validators) when is_map(Validators) ->
  Result = maps:fold(fun(Key, Validator, Acc) when is_function(Validator, 1) ->
    case Validator(maps:get(Key, Data, undefined)) of
      undefined -> Acc;
      { value, Value } ->
        [{ Key,Value } | Acc ]
    end
  end, [], Validators),
  maps:from_list(Result).

to_integer(Binary) when is_binary(Binary) -> binary_to_integer(Binary);
to_integer(Int) when is_integer(Int) -> Int.

to_boolean(Binary) when is_binary(Binary) ->
  case Binary of
    <<"true">> -> true;
    <<"false">> -> false
  end;
to_boolean(Int) when is_integer(Int) -> Int /= 0;
to_boolean(Boolean) when is_boolean(Boolean) -> Boolean.

definition(Fun) ->
  { module, Module } = erlang:fun_info(Fun, module),
  { name, Name } = erlang:fun_info(Fun, name),
  { arity, Arity } = erlang:fun_info(Fun, arity),
  { Module, Name, Arity }.

ensure_function_exported({ FunModule, FunName, Arity } = Info) ->
  case erlang:function_exported(FunModule, FunName, Arity) of
    true -> Info;
    false -> error({function_not_exported, Info})
  end;
ensure_function_exported(Fun) when is_function(Fun) ->
  Info = calypso_util:definition(Fun),
  ok = ensure_function_exported(Info),
  Info.
