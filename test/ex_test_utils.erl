-module(ex_test_utils).

-export([publish_physical_host_args/1,
         physical_port/3,
         patch_panel/3,
         physical_host/1,
         part_of_link/2]).

publish_physical_host_args(string) ->
    [_ClPatchP = "ClPatchPanel",
     _Ph = "PH1",
     _Ports = [{"PP1", #{mac_address => "MAC1", interface => eth0}},
               {"PP2", #{mac_address => "MAC2", interface => eth1}}]];
publish_physical_host_args(binary) ->
    [ClPatchP, Ph, Ports] = publish_physical_host_args(string),
    [ex_dby_lib:binarize(identifier, X) || X <- [ClPatchP, Ph]]
        ++ [ex_dby_lib:binarize(phy_ports, Ports)].

patch_panel(PhName, Name, AttachedPorts) ->
    Wires = maps:from_list([{P, null} || P <- AttachedPorts]),
    {prefix(PhName, Name), [{<<"type">>, <<"lm_patchp">>},
                            {<<"wires">>, Wires}]}.

physical_host(Name) ->
    {Name, [{<<"type">>, <<"lm_ph">>}]}.

physical_port(PhName, Name, Properties) ->
    {prefix(PhName, Name), [{<<"type">>, <<"lm_pp">>} | Properties]}.

part_of_link(A, B) ->
    {A, B, [{<<"type">>, <<"part_of">>}]}.

prefix(<<>>, Name) ->
    <<Name/binary>>;
prefix(Prefix, Name) ->
    <<Prefix/binary, "/", Name/binary>>.
