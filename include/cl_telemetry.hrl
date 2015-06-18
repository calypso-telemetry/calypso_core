-author("begemot").

-record(telemetry, {
  type = raw :: cl_telemetry:type(),
  time_start = undefined :: term(),
  time_end = undefined :: term(),
  data = maps:new() :: maps:maps()
}).

-define(IS_TELEMETRY(Telemetry), is_record(Telemetry, telemetry)).
-define(TELEMETRY_TYPE(Telemetry), Telemetry#telemetry.type).
-define(TELEMETRY_PERIOD(Telemetry), { Telemetry#telemetry.time_start, Telemetry#telemetry.time_end }).
