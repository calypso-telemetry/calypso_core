-module(cl_device).
-author("begemot").

-include("cl_device.hrl").

%% API
-export([
  new/3, new/2,
  new_internal/3, new_internal/4, create/1,
  id/1, object_id/1, module/1, is_device/1,
  login/1, set_login/2,
  is_equal/2, is_same/2,
  telemetry/2, telemetry/1,
  info/1, info/2, info/3, set_info/2, set_info/3, info_merge/2,
  is_active/1, set_active/2,
  merge/2
]).

-type id() :: binary().
-type device() :: #device{}.

-export_type([ id/0, device/0 ]).

-callback new(Id :: binary() | undefined, Data :: maps:maps()) -> { ok, device() }.
-callback handle_update_info(Info :: maps:map(), device()) -> { ok, device()}|undefined.

is_device(Device) -> ?IS_DEVICE(Device).

new(Id, Module, Info) when is_atom(Module) ->
  Module:new(Id, Info).

new(Module, Info) when is_atom(Module) ->
  new(undefined, Module, Info).

new_internal(Id, Module, Info, DefaultInfo) when (?IS_DEVICE_ID(Id) orelse Id =:= undefined), is_atom(Module), is_map(Info), is_map(DefaultInfo) ->
  D0 = #device{
    id = Id,
    module = Module
  },
  D1 = update_info(#{}, DefaultInfo, D0),
  info_merge(Info, D1).

new_internal(Id, Module, Info) when is_atom(Module) ->
  new_internal(Id, Module, Info, #{});

new_internal(Module, Info, DefaultInfo) when is_atom(Module), is_map(Info), is_map(DefaultInfo) ->
  new_internal(undefined, Module, Info, DefaultInfo).

is_equal(Device1, Device2) ->
  is_active(Device1) =:= is_active(Device2) andalso is_same(Device1, Device2) andalso info(Device1) =:= info(Device2).

is_same(Device1, Device2) ->
  id(Device1) =:= id(Device2) andalso
  module(Device1) =:= module(Device2).

id(Device) ->
  Device#device.id.

object_id(Device) ->
  Device#device.object_id.

telemetry(Device) ->
  Device#device.current_telemetry.

module(Device) ->
  Device#device.module.

info(Device) ->
  Device#device.info.

info(Key, Device) ->
  maps:get(Key, info(Device)).

info(Key, Device, Default) ->
  maps:get(Key, info(Device), Default).

set_info(Key, Value, Device) ->
  update_info(info(Device), maps:put(Key, Value, maps:new()), Device).

info_merge(Info, Device) when is_map(Info) ->
  update_info(info(Device), Info, Device).

set_info(Info, Device) when is_map(Info) ->
  update_info(#{}, Info, Device).

is_active(Device) ->
  Device#device.is_active.

set_active(Activation, Device) when is_boolean(Activation) ->
  Device#device{
    is_active = Activation
  }.

merge(DeviceNew, DeviceOld) ->
  true = is_same(DeviceOld, DeviceNew),
  NewDevice0 = telemetry(telemetry(DeviceNew), DeviceOld),
  NewDevice1 = info_merge(info(DeviceNew), NewDevice0),
  set_active(is_active(DeviceNew), NewDevice1).

telemetry(NewTelemetry, Device) ->
  { _ , TimeEnd } = cl_telemetry:period(NewTelemetry),
  T1 = cl_telemetry:as_current(TimeEnd, telemetry(Device)),
  T2 = cl_telemetry:as_current(TimeEnd, NewTelemetry),
  NewDevice = Device#device{
    current_telemetry = cl_telemetry:merge(T1, T2)
  },
  calypso_telemetry_hooks:telemetry_fire({ device, Device, NewDevice}, NewTelemetry),
  NewDevice.

login(Device) ->
  Device#device.login.

set_login(Login, Device) when is_binary(Login) ->
  Device#device{
    login = Login
  }.

create(#device{ id = undefined } = Device) ->
  {ok, NewId } = calypso_db:create(device, Device),
  Device#device{ id = NewId }.

%% -------------------------------------------------------------------------------------------
%% Internal
%% -------------------------------------------------------------------------------------------

update_info(OldInfo, NewInfo, Device) when is_map(OldInfo), is_map(NewInfo) ->
  { Info, NewDevice } = maps:fold(fun
    (is_active, undefined, Acc) -> Acc;
    (is_active, Value, { I, D }) ->
      { I, set_active(calypso_util:to_boolean(Value), D) };
    (id, _, Acc) -> Acc;
    (module, _, Acc) -> Acc;
    (login, Login, { I, D }) ->
      { I, cl_device:set_login(Login, D) };
    (Key, undefined, { I, D }) ->
      { maps:remove(Key, I), D };
    (Key, Value, { I, D }) ->
      { maps:put(Key, Value, I), D }
  end, { OldInfo, Device }, NewInfo),
  Module = module(NewDevice),
  case Module:handle_update_info(Info, NewDevice) of
    { ok, NDevice } when ?IS_DEVICE(NDevice) -> NDevice;
    undefined ->
      NewDevice#device{
        info = Info
      }
  end.
