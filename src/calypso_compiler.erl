-module(calypso_compiler).
-author("Sergey Loguntsov").

%% API
-export([
  compile_term/4
]).

compile_term(Module, FunName, T, Default) ->
  Forms = term_to_abstract(Module, FunName, T, Default),
  { ok, Module, Bin } = compile:forms(Forms, [verbose, report_errors]),
  code:purge(Module),
  { module, Module } = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin),
  ok.

term_to_abstract(Module, Getter, T, Default) when is_map(T) ->
    A = erl_syntax:variable("A"),
    Forms = [%% -module(Module).
     erl_syntax:attribute(
       erl_syntax:atom(module),
       [erl_syntax:atom(Module)]),
     %% -export([Getter/1]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(Getter),
            erl_syntax:integer(1))])]),
     %% Getter(Key) -> Value.
     erl_syntax:function(
       erl_syntax:atom(Getter),
       [erl_syntax:clause([A], none, [
        erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(maps), erl_syntax:atom(get)),
          [A,erl_syntax:abstract(T), erl_syntax:abstract(Default)])
       ])])
    ],
    [ erl_syntax:revert(X) || X <- Forms ].
