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
    expect_dby_lib(init, 0, ok),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = meck:unload(ex_dby_lib).

all() ->
    [it_publishes_physical_host,
     it_publishes_virtual_host].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

it_publishes_physical_host(_Config) ->
    ?GIVEN(begin
               publisher_server_is_running(),
               [ClPatchP, Ph, Ports] = publish_ph_args(string),
               [ClPatchPBin, PhBin, PortsBin] = publish_ph_args(binary),
               [expect_dby_lib(identifier_exists, [X], false)
                || X <- [ClPatchPBin, PhBin]],
               expect_dby_lib(publish, 2, ok)
           end),

    ?WHEN(ex_publisher:publish_physical_host(ClPatchP, Ph, Ports)),

    ?THEN(begin
              PortsDbyEps = [physical_port_dby_ep(PhBin, PBin, Prop)
                             || {PBin, Prop} <- PortsBin],
              AttachedPortsBin = element(1, lists:unzip(PortsDbyEps)),
              {PhPatchpBin, _} = PhPatchpDbyEp =
                  patch_panel_dby_ep(PhBin, <<"PatchP">>, AttachedPortsBin),
              Links1 = [
                       [part_of_dby_link(IdBin, P) || {P, _} <- PortsDbyEps]
                       || IdBin <- [ClPatchPBin, PhBin, PhPatchpBin]
                       ],
              Links2 = [part_of_dby_link(PhPatchpBin, PhBin) | Links1],
              ExpectedPublish =
                  [patch_panel_dby_ep(<<>>, ClPatchPBin, AttachedPortsBin),
                   physical_host_dby_ep(PhBin),
                   PhPatchpDbyEp
                   | lists:flatten(Links2 ++ PortsDbyEps)],
              ok = meck:wait(ex_dby_lib, publish, '_', 1000),
              ActualPublish = meck:capture(first, ex_dby_lib, publish, '_', 2),
              ?assertEqual(lists:sort(ExpectedPublish),
                           lists:sort(ActualPublish))
          end).

it_publishes_virtual_host(_Config) ->
    ?GIVEN(begin
               publisher_server_is_running(),
               [Ph, Vh, VirtPorts, VifPorts] = publish_vh_args(string),
               [PhBin, VhBin, VirtPortsBin, VifPortsBin] = publish_vh_args(binary),
               PhPatchpBin = <<PhBin/binary, "/Patchp">>,
               [expect_dby_lib(Fun, Args, Ret)
                || {Fun, Args, Ret} <-
                       [{identifier_is_physical_host, [PhBin], true},
                        {identifier_exists, [VhBin], false},
                        {host_patch_panel, [PhBin], PhPatchpBin},
                        {publish, 2, ok}]]
           end),

    ?WHEN(ex_publisher:publish_virtual_host(Ph, Vh, VirtPorts, VifPorts)),

    ?THEN(begin
              VirtPortsDbyEps = [virtual_port_dby_ep(VhBin, PBin, Prop)
                                 || {PBin, Prop} <- VirtPortsBin],
              VifPortsDbyEps = [vif_port_dby_ep(VhBin, PBin, Prop)
                                 || {PBin, Prop} <- VifPortsBin],
              VirtPortsDbyIds = element(1, lists:unzip(VirtPortsDbyEps)),
              VifPortsDbyIds = element(1, lists:unzip(VifPortsDbyEps)),
              {VhPatchpBin, _} = VhPatchpDbyEp =
                  patch_panel_dby_ep(VhBin, <<"PatchP">>, VirtPortsDbyIds),
              Links1 =  [
                         [part_of_dby_link(IdBin, P) || P <- VirtPortsDbyIds]
                         || IdBin <- [VhPatchpBin, VhBin]
                        ]
                  ++ [part_of_dby_link(VhBin, PhBin),
                      part_of_dby_link(VhPatchpBin, VhBin)
                      | [part_of_dby_link(PhPatchpBin, P) || P <- VifPortsDbyIds]],
              {VifPortsDbyEps1, Links2} =
                  lists:mapfoldl(
                    fun({P, Props0}, Links) ->
                            {ToBound, Props1} =
                                retrieve_virt_port_to_bound_for_vif(Props0),
                            {{P, Props1}, [bound_to_dby_link(ToBound, P), Links]}
                    end, Links1, VifPortsDbyEps),
              ExpectedPublish0 =
                  [VhPatchpDbyEp,
                   virtual_host_dby_ep(VhBin)
                   | lists:flatten(VirtPortsDbyEps ++ VifPortsDbyEps1 ++ Links2)],
              ok = meck:wait(ex_dby_lib, publish, '_', 5000),
              ActualPublish = meck:capture(first, ex_dby_lib, publish, '_', 2),
              {value, {_, ActualFun}, ActualPublish1} =
                  lists:keytake(PhPatchpBin, 1, ActualPublish),
              ?assert(is_function(ActualFun, 1)),
              ExpectedPublish1 = lists:sort(ExpectedPublish0),
              ?assertEqual(ExpectedPublish1, lists:sort(ActualPublish1))
          end).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

publisher_server_is_running() ->
    {ok, _Pid} = ex_publisher:start_link().

expect_dby_lib(Fun, Args, Ret) ->
    ok = meck:expect(ex_dby_lib, Fun, Args, Ret).

publish_ph_args(Type) ->
    ex_test_utils:publish_physical_host_args(Type).

publish_vh_args(Type) ->
    ex_test_utils:publish_virtual_host_args(Type).

physical_port_dby_ep(Ph, Port, PortProperties) ->
    ex_test_utils:physical_port(Ph, Port, PortProperties).

virtual_port_dby_ep(Vh, Port, PortProperties) ->
    ex_test_utils:virtual_port(Vh, Port, PortProperties).

vif_port_dby_ep(Vh, Port, PortProperties) ->
    ex_test_utils:vif_port(Vh, Port, PortProperties).

patch_panel_dby_ep(Ph, PatchP, AttachedPorts) ->
    ex_test_utils:patch_panel(Ph, PatchP, AttachedPorts).

retrieve_virt_port_to_bound_for_vif(VifProperties) ->
    ex_test_utils:retrieve_virt_port_to_bound_for_vif(VifProperties).

physical_host_dby_ep(Ph) ->
    ex_test_utils:physical_host(Ph).

virtual_host_dby_ep(Vh) ->
    ex_test_utils:virtual_host(Vh).

part_of_dby_link(Src, Dst) ->
    ex_test_utils:part_of_link(Src, Dst).

bound_to_dby_link(Src, Dst) ->
    ex_test_utils:bound_to_link(Src, Dst).














