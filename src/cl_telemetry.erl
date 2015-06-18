-module(cl_telemetry).
-author("begemot").

-include("cl_telemetry.hrl").

-type telemetry() :: #telemetry{}.
-type telemetry_list() :: [ telemetry() ].
-type type() :: raw.

-export_type([ telemetry/0, telemetry_list/0, type/0 ]).

%% API
-export([
  new/0, new/2, new/3,
  period/1, set_period/2, as_current/2,
  type/1, data/1, data/2, data/3,
  merge/2, apply/2,
  diff/2,
  time_end/1,
  filter_by_keys/2
]).

new() ->
  #telemetry{}.

new(Time, Data) when is_map(Data) ->
  #telemetry{
    type = raw,
    time_end = Time,
    data = Data
  }.

new(Type, Time, Data) when is_atom(Type), is_map(Data), is_integer(Time) ->
  #telemetry{
    type = Type,
    time_end = Time,
    data = Data
  }.

period(Telemetry) ->
  ?TELEMETRY_PERIOD(Telemetry).

type(Telemetry) ->
  ?TELEMETRY_TYPE(Telemetry).

data(Telemetry) ->
  Telemetry#telemetry.data.

data(Key, Telemetry) ->
  maps:get(Key, data(Telemetry)).

data(Key, Telemetry, Default) ->
  maps:get(Key, data(Telemetry), Default).

time_end(Telemetry) ->
  Telemetry#telemetry.time_end.

merge(Telemetry1, Telemetry2) when ?TELEMETRY_TYPE(Telemetry1) =:= ?TELEMETRY_TYPE(Telemetry2), ?TELEMETRY_PERIOD(Telemetry1) =:= ?TELEMETRY_PERIOD(Telemetry2) ->
  Telemetry2#telemetry{
    data = maps:merge(data(Telemetry1), data(Telemetry2))
  };
merge(T1, T2) ->
  error(badarg, [ T1, T2 ]).

diff(TelemetryOld, TelemetryNew) ->
  NewData = maps:fold(fun(Key, OldValue, Data) ->
    case maps:get(Key, Data, '$undefined') of
      '$undefined' -> Data;
      NewValue when OldValue =:= NewValue -> maps:remove(Key, Data);
      _ -> Data
    end
  end, data(TelemetryNew), data(TelemetryOld)),
  TelemetryNew#telemetry{
    data = NewData
  }.

set_period({TimeStart, TimeEnd}, Telemetry) when TimeStart =< TimeEnd; TimeStart =:= undefined ->
  Telemetry#telemetry{
    time_start = TimeStart,
    time_end = TimeEnd
  };
set_period(undefined, Telemetry) ->
  set_period({undefined, undefined}, Telemetry).

as_current(TimeEnd, Telemetry) ->
  set_period({ undefined, TimeEnd}, Telemetry).

apply(Fun, Telemetry) ->
  NewData = Fun(Telemetry),
  case is_map(NewData) of
    true -> ok;
    false -> error(bad_function, [ NewData, Telemetry ])
  end,
  Telemetry#telemetry{
    data = NewData
  }.

filter_by_keys([], Telemetry) ->
  Telemetry#telemetry{
    data = #{}
  };
filter_by_keys(Keys, Telemetry) when is_list(Keys) ->
  Data = maps:fold(fun(Key, Value, Acc) ->
    case lists:member(Key, Keys) of
      true -> [{ Key, Value} |  Acc ];
      false -> Acc
    end
  end, [], data(Telemetry)),
  Telemetry#telemetry{
    data = maps:from_list(Data)
  }.
