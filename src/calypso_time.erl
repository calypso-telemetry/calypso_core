-module(calypso_time).
-author("begemot").

%% API
-export([
  now/0, diff/2, from_timestamp/1, add/2, from_lifetime/1, is_infinity/1, less_than/3, less/2,
  sleep/1, from_datetime/1, minute/1,
  to_sql/2, to_sql_now/1,from_sql/2
]).

-define(UNIX_EPOH, 62167219200).

-type(time() :: integer()|infinity).
-export_type([time/0]).

-spec now() -> time().
now() ->
  from_timestamp(os:timestamp()).

diff(undefined, _ ) -> undefined;
diff(infinity, _Time2) -> infinity;
diff(Time1, Time2) when is_integer(Time1), is_integer(Time2) ->
  Time1 - Time2.

add(infinity, _) -> infinity;
add(_, infinity) -> infinity;
add(A,B) ->
  A + B.

from_lifetime(LifeTime) ->
  add(?MODULE:now(), LifeTime).

-spec from_timestamp(erlang:timestamp()) -> time().
from_timestamp({Mega, Sec, _Micro}) ->
 Mega * 1000000 + Sec. %% 62167219200 -- грегорианские секунды для 1.01.1970

is_infinity(Time) ->
  Time =:= infinity.

sleep(Seconds) when is_integer(Seconds) ->
  timer:sleep(Seconds * 1000).

less_than(infinity, infinity, _) -> true;
less_than(infinity, _, _) -> false;
less_than(_, infinity, _) -> false;
less_than(_, _, infinity) -> false;
less_than(Time1, Time2, Seconds) when is_integer(Seconds) ->
  Time = diff(Time1, Time2),
  Time =< Seconds.

less(Time1, Time2) ->
  less_than(Time1, Time2, 0).

from_datetime({{_,_,_},{_,_,_}} = DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOH.

minute(infinity) -> infinity;
minute(Time) ->
  Time div 60.

to_sql(Type, Time) when is_atom(Type), is_integer(Time) ->
  {{Year, Month, Day}, { Hour, Minute, Seconds }} = calendar:gregorian_seconds_to_datetime(Time + ?UNIX_EPOH),
  Result = case Type of
    datetime ->
      io_lib:format("~p-~p-~p ~p:~p:~p", [ Year, Month, Day, Hour, Minute, Seconds ]);
    date ->
      io_lib:format("~p-~p-~p", [ Year, Month, Day ])
  end,
  list_to_binary(lists:flatten(Result)).

to_sql_now(Type) ->
  to_sql(Type, ?MODULE:now()).
from_sql(datetime, Binary) when is_binary(Binary) ->
  [ BDate, BTime ] = binary:split(Binary, <<" ">>),
  [ BYear, BMonth, BDay ] = binary:split(BDate, <<"-">>, [ global ]),
  [ BHour, BMinutes, BSec ] = binary:split(BTime, <<":">>, [ global ]),
  from_datetime({
    { binary_to_integer(BYear), binary_to_integer(BMonth), binary_to_integer(BDay) },
    { binary_to_integer(BHour), binary_to_integer(BMinutes), binary_to_integer(BSec) }
  });

from_sql(date, Binary) when is_binary(Binary) ->
  from_sql({date, { 0,0,0 }}, Binary);

from_sql({date, { _Hour, _Minutes, _Secs } = Time}, Binary) when is_binary(Binary) ->
  [ BYear, BMonth, BDay ] = binary:split(Binary, <<"-">>, [ global ]),
  from_datetime({
    { binary_to_integer(BYear), binary_to_integer(BMonth), binary_to_integer(BDay) },
    Time
  });

from_sql(_, { datetime, Time}) ->
  from_datetime(Time).
