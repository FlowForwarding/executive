-module(ex_publisher_SUITE).

-compile(export_all).

-define(GIVEN(X), X).
-define(WHEN(X), X).
-define(THEN(X), X).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


suite() ->
    [{timetrap,{minutes,10}}].

init_per_testcase(_TestCase, Config) ->
    ok = meck:new(ex_dby_lib, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = meck:unload(ex_dby_lib).

all() ->
    [it_publishes_physical_host].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

it_publishes_physical_host(_Config) ->
    ?GIVEN(begin
               publisher_server_is_running(),
               [ClPatchP, Ph, Ports] = publish_ph_args(string),
               [ClPatchPBin, PhBin, PortsBin] = publish_ph_args(binary),
               [expect_dby_client(identifier_exists, [X], false)
                || X <- [ClPatchPBin, PhBin]],
               expect_dby_client(publish, 2, ok)
           end),

    ?WHEN(ex_publisher:publish_physical_host(ClPatchP, Ph, Ports)),

    ?THEN(begin
              PortsDbyEps = [physical_port_dby_ep(PhBin, PBin, Prop)
                             || {PBin, Prop} <- PortsBin],
              AttachedPortsBin = element(1, lists:unzip(PortsDbyEps)),
              {PhPatchpBin, _} = PhPatchpDbyEp =
                  patch_panel_dby_ep(PhBin, <<"PatchP">>, AttachedPortsBin),
              Links = [
                       [part_of_dby_link(IdBin, P) || {P, _} <- PortsDbyEps]
                       || IdBin <- [ClPatchPBin, PhBin, PhPatchpBin]
                      ],
              ExpectedPublish =
                  [patch_panel_dby_ep(<<>>, ClPatchPBin, AttachedPortsBin),
                   physical_host_dby_ep(PhBin),
                   PhPatchpDbyEp
                   | lists:flatten(Links ++ PortsDbyEps)],
              ok = meck:wait(ex_dby_lib, publish, '_', 1000),
              ActualPublish = meck:capture(first, ex_dby_lib, publish, '_', 2),
              ?assertEqual(lists:sort(ExpectedPublish),
                           lists:sort(ActualPublish))
          end).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

publisher_server_is_running() ->
    {ok, _Pid} = ex_publisher:start_link().

expect_dby_client(Fun, Args, Ret) ->
    ok = meck:expect(ex_dby_lib, Fun, Args, Ret).

publish_ph_args(Type) ->
    ex_test_utils:publish_physical_host_args(Type).

prefix(Prefix, Name) ->
    Prefix ++ "/" ++ Name.

physical_port_dby_ep(Ph, Port, PortProperties) ->
    ex_test_utils:physical_port(Ph, Port, PortProperties).

patch_panel_dby_ep(Ph, PatchP, AttachedPorts) ->
    ex_test_utils:patch_panel(Ph, PatchP, AttachedPorts).

physical_host_dby_ep(Ph) ->
    ex_test_utils:physical_host(Ph).

part_of_dby_link(Src, Dst) ->
    ex_test_utils:part_of_link(Src, Dst).




