-author("Sergey Loguntsov").

-record(device, {
  id :: cl_device:id(),
  login :: binary(),
  object_id :: cl_object:id(),
  module :: atom(),
  current_telemetry = cl_telemetry:new() :: cl_telemetry:telemetry(),
  info = #{} :: maps:map(),
  is_active = false :: boolean()
}).

-define(IS_DEVICE(Sensor), is_record(Sensor, device)).
-define(IS_DEVICE_ID(Id) , is_binary(Id)).
