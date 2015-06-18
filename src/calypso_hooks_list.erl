-module(calypso_hooks_list).
-author("begemot").

%% API
-export([
  get_hooks/1
]).

get_hooks(Name) ->
  case ets:lookup(hooks, Name) of
    [{ _, H}] -> H;
    [] -> []
  end.
