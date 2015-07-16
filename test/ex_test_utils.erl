-module(ex_test_utils).

-export([publish_physical_host_args/1,
         publish_virtual_host_args/1,
         publish_of_switch_args/1,
         physical_port/3,
         virtual_port/3,
         vif_port/3,
         of_port/4,
         retrieve_virt_port_to_bound_for_vif/1,
         patch_panel/3,
         physical_host/1,
         virtual_host/1,
         of_switch/1,
         part_of_link/2,
         bound_to_link/2]).

publish_physical_host_args(string) ->
    [_ClPatchP = "ClPatchPanel",
     _Ph = "PH1",
     _Ports = [{"PP1", #{mac_address => "MAC1", interface => eth0}},
               {"PP2", #{mac_address => "MAC2", interface => eth1}}]];
publish_physical_host_args(binary) ->
    [ClPatchP, Ph, Ports] = publish_physical_host_args(string),
    [ex_dby_lib:binarize(identifier, X) || X <- [ClPatchP, Ph]]
        ++ [ex_dby_lib:binarize(ports, Ports)].

publish_virtual_host_args(string) ->
    [_Ph = "PH1",
     _Vh = "PH1/VH1",
     _VirtualPorts = [{"VP1", #{mac_address => "MAC_VP1", interface => eth0}},
                      {"VP2", #{mac_address => "MAC_VP2", interface => eth1}}],
     _VifPorts = [{"VP1.1", #{mac_address => "MAC_VP1.1",
                              interface => 'vif1.1',
                              vp_to_bound => "VP1"}},
                  {"VP1.2", #{mac_address => "MAC_VP2.1",
                              interface => 'vif1.2',
                              vp_to_bound => "VP2"}}]];
publish_virtual_host_args(binary) ->
    [Ph, Vh, VirtPorts, VifPorts] = publish_virtual_host_args(string),
    [ex_dby_lib:binarize(identifier, X) || X <-[Ph, Vh]]
        ++ [ex_dby_lib:binarize(ports, P) || P <- [VirtPorts, VifPorts]].

publish_of_switch_args(string) ->
    [_Vh = "PH1/VH1",
     _OFS = "PH/VH1/OFS1",
     _OPorts = [{"OFP1", #{vp_to_bound => "VP1"}},
                {"OFP2", #{vp_to_bound => "VP2"}}]];
publish_of_switch_args(binary) ->
    [Vh, Ofs, OfPorts] = publish_of_switch_args(string),
    [ex_dby_lib:binarize(identifier, X) || X <-[Vh,Ofs]]
        ++ [ex_dby_lib:binarize(ports, OfPorts)].

patch_panel(PhName, Name, AttachedPorts) ->
    Wires = maps:from_list([{P, null} || P <- AttachedPorts]),
    {prefix(PhName, Name), [{<<"type">>, <<"lm_patchp">>},
                            {<<"wires">>, Wires}]}.

physical_host(Name) ->
    {Name, [{<<"type">>, <<"lm_ph">>}]}.

virtual_host(Name) ->
    {Name, [{<<"type">>, <<"lm_vh">>}]}.

of_switch(Name) ->
    {Name, [{<<"type">>, <<"lm_of_switch">>}]}.

physical_port(PhName, Name, Properties) ->
    {prefix(PhName, Name), [{<<"type">>, <<"lm_pp">>} | Properties]}.

virtual_port(VhName, Name, Properties) ->
    {prefix(VhName, Name), [{<<"type">>, <<"lm_vp">>} | Properties]}.

vif_port(VhName, Name, Properties0) ->
    VP = proplists:get_value(K = <<"vp_to_bound">>, Properties0),
    Properties1 = [{K, prefix(VhName, VP)}
                   | proplists:delete(<<"vp_to_bound">>, Properties0)],
    {prefix(VhName, Name), [{<<"type">>, <<"lm_vp">>} | Properties1]}.

of_port(OfsName, VhName, Name, Properties0) ->
    VP = proplists:get_value(K = <<"vp_to_bound">>, Properties0),
    Properties1 = [{K, prefix(VhName, VP)}
                   | proplists:delete(<<"vp_to_bound">>, Properties0)],
    {prefix(OfsName, Name), [{<<"type">>, <<"lm_of_port">>} | Properties1]}.

retrieve_virt_port_to_bound_for_vif(Properites) ->
    K = <<"vp_to_bound">>,
    {proplists:get_value(K, Properites), proplists:delete(K, Properites)}.

part_of_link(A, B) ->
    {A, B, [{<<"type">>, <<"part_of">>}]}.

bound_to_link(A, B) ->
    {A, B, [{<<"type">>, <<"bound_to">>}]}.

prefix(<<>>, Name) ->
    <<Name/binary>>;
prefix(Prefix, Name) ->
    <<Prefix/binary, "/", Name/binary>>.
