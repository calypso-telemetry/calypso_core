-module(calypso_hooks).
-author("begemot").

-include("logger.hrl").

-define(SERVER, ?MODULE).
-define(ETS, hooks).

%% API
-export([
  start_link/0,
  compile/0,
  add/3, delete/2,
  add_hook/2, delete_hook/2,
  run/2,
  run_boolean_all/2, run_boolean_all/3,
  run_boolean_any/2, run_boolean_any/3,
  run_map/2, run_map/3,
  run_filter/3,
  run_with_predicate/4,
  run_while_with_predicate/4,
  run_before_result/2
]).

-export([
  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-record(state, {}).

start_link() ->
  ?ETS = ets:new(?ETS, [ named_table, set, public, {write_concurrency, false}, {read_concurrency, true}]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Name, Id, Function) ->
  add(Name, undefined, Id, Function).

add(Name, undefined, { Id, Priority }, Function) when is_atom(Name) ->
  add(Name, Priority, Id, Function);
add(Name, Priority, Id, Function) when is_function(Function, 1), is_atom(Name),is_atom(Id) ->
  { FunModule, FunName, 1 } = definition(Function),
  case erlang:function_exported(FunModule, FunName, 1) of
    true -> ok;
    false -> error({function_not_exported, [ FunModule, FunName, 1 ]})
  end,
  F = { FunModule, FunName },
  ok = gen_server:call(?SERVER, { add, Name, Priority, Id, { FunModule, FunName } }),
  run(core_add_hook, { Name, Priority, Id }),
  ok.

delete(Name, Id) ->
  ok = gen_server:call(?SERVER, { delete, Name, Id }),
  run(core_delete_hook, { Name, Id }).

run(Name, Arg) ->
  ?INFO("Hook ~p fire. Arg ~p", [ Name, Arg ]),
  lists:foreach(fun({_, Id, Func}) ->
    run_hook(Name, Id, Func, Arg)
  end, get_hooks(Name)),
  ok.

run_boolean_all(Name, Arg) ->
  run_with_predicate(Name, Arg, true, fun(_, Item, Acc) -> Acc andalso Item end).

run_boolean_all(Name, Arg, Function) when is_function(Function, 1) ->
  run_with_predicate(Name, Arg, true, fun(_, Item, Acc) -> Acc andalso Function(Item) end).

run_boolean_any(Name, Arg, Function) when is_function(Function, 1) ->
  run_with_predicate(Name, Arg, false, fun(_, Item, Acc) -> Acc orelse Function(Item) end).

run_boolean_any(Name, Arg) ->
  run_with_predicate(Name, Arg, false, fun(_, Item, Acc) -> Acc orelse Item end).

run_map(Name, Arg, Function)  when is_function(Function, 1) ->
  run_with_predicate(Name, Arg, [], fun(Id, Result, Acc) -> [ { Id, Function(Result)} | Acc ] end).

run_map(Name, Arg) ->
  run_with_predicate(Name, Arg, [], fun(Id, Result, Acc) -> [ {Id, Result} | Acc ] end).

run_filter(Name, Arg, Function) when is_function(Function, 1) ->
  run_with_predicate(Name, Arg, [], fun(_, Item, Acc) ->
    case Function(Item) of
      true -> [ Item | Acc ];
      false -> Acc
    end
  end).

run_before_result(Name, Arg) ->
  run_while_with_predicate(Name, Arg, undefined, fun(Id, Result, Acc) ->
    case Result of
      { ok, R } ->
        { false, { ok, Id, R } };
      { value, R } ->
        { false, { value, Id, R }};
      _ -> { true, Acc }
    end
  end).

run_with_predicate(Name, Arg, State, Predicate) when is_function(Predicate, 3) ->
  run_while_with_predicate(Name, Arg, State, fun(Id, Result, Acc) -> { true, Predicate(Id, Result, Acc) } end).

run_while_with_predicate(Name, Arg, State, Predicate) when is_function(Predicate, 3) ->
  ?INFO("Hook ~p fire. Arg ~p", [ Name, Arg ]),
  run_while_loop(Name, Arg, State, Predicate, get_hooks(Name)).

run_while_loop(_Name, _Arg, State, _Predicate, []) -> State;
run_while_loop(Name, Arg, State, Predicate, [{_, Id, Function}|Hooks]) ->
  case run_hook(Name, Id, Function, Arg) of
    { ok, Result } ->
      case Predicate(Id, Result, State) of
        { true, NewState } ->
          run_while_loop(Name, Arg, NewState, Predicate, Hooks);
        { false, NewState } ->
          NewState
      end;
    _ ->
      run_while_loop(Name, Arg, State, Predicate, Hooks)
  end.

init([]) ->
  { ok, #state{} }.

handle_call({add, Name, Priority, Id, Func}, _From, State) ->
  Result = handle_add(Name, Priority, Id, Func),
  { reply, Result, State };

handle_call({delete, Name, Id}, _From, State) ->
  Result = handle_delete(Name, Id),
  { reply, Result, State };

handle_call(_, _From, State) ->
  { reply, error, State }.

handle_cast(_, State) ->
  { noreply, State }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_add(Name, Priority, Id, Func) ->
  Hooks = get_hooks(Name),
  NewPriority = case Priority of
    undefined ->
      lists:max([ 10000 | [ element(1, Hook) || Hook <- Hooks ]]) + 10;
    Any -> Any
  end,
  NewHooks = lists:sort([ {NewPriority, Id, Func } | Hooks]),
  set_hooks(Name, NewHooks),
  ok.

handle_delete(Name, Id) ->
  Hooks = get_hooks(Name),
  NewHooks = lists:filter(fun({_, I, _ }) -> I =/= Id end, Hooks),
  set_hooks(Name, NewHooks),
  ok.

get_hooks(Name) ->
  calypso_hooks_list:get_hooks(Name).

set_hooks(Name, Hooks) ->
  ets:insert(?ETS, { Name, Hooks}).

run_hook(Name, Id, { FunModule, FunName }, Arg) when is_atom(FunModule), is_atom(FunName) ->
  Key = {run_hook, Name, Id},
  case get(Key) of
    true ->
      { error, unknown };
    undefined ->
      put(Key, true),
      R = case catch erlang:apply(FunModule, FunName, [ Arg ]) of
        { 'EXIT', { function_clause, [_, { ?MODULE, run_hook, 4 ,_ } |_] }} ->
          { error, unknown };
        { 'EXIT', Reason } ->
          ?ERROR("Error when hook ~p (~p) launched with arg ~p reason ~p", [ Name, Id, Arg, Reason ]),
          { error, Reason };
        Any ->
          { ok, Any }
      end,
      erase(Key),
      R
  end;

run_hook(Name, Id, Function, Arg) when is_function(Function, 1)->
  Key = {run_hook, Name, Id},
  case get(Key) of
    true ->
      { error, unknown };
    undefined ->
      put(Key, true),
      R = case catch Function(Arg) of
        { 'EXIT', { function_clause, [_, { ?MODULE, run_hook, 4 ,_ } |_] }} ->
          { error, unknown };
        { 'EXIT', Reason } ->
          ?ERROR("Error when hook ~p (~p) launched with arg ~p reason ~p", [ Name, Id, Arg, Reason ]),
          { error, Reason };
        Any ->
          { ok, Any }
      end,
      erase(Key),
      R
  end.


compile() ->
  AllValues = maps:from_list(ets:foldl(fun(Item, Acc) ->
    [ Item | Acc ]
  end, [], ?ETS)),
  ok = calypso_compiler:compile_term(calypso_hooks_list, get_hooks, AllValues, []),
  ets:delete(?ETS),
  ok.

definition(Fun) ->
  { module, Module } = erlang:fun_info(Fun, module),
  { name, Name } = erlang:fun_info(Fun, name),
  { arity, Arity } = erlang:fun_info(Fun, arity),
  { Module, Name, Arity }.

add_hook(Id, Fun) ->
  add(core_add_hook, Id, Fun).

delete_hook(Id, Fun) ->
  add(core_delete_hook, Id, Fun).
