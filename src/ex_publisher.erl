-module(ex_publisher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(PUBLISHER, <<"executive">>).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         publish_physical_host/3,
         publish_virtual_host/4,
         publish_of_switch/3,
         publsh_endpoint/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Records && Includes
%% ------------------------------------------------------------------

-record(state, {}).

-include("executive.hrl").
-include("ex_logger.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec publish_physical_host(string(), string(), phy_ports()) ->
                                   ok | {error, term()}.

publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports) ->
    Req = {pub_phy_host, ClusterPatchPanel, PhysicalHost, Ports},
    gen_server:call(?SERVER, Req).


-spec publish_virtual_host(string(), string(), virt_ports(), vif_ports()) ->
                                  ok | {error, term()}.

publish_virtual_host(PhysicalHost, VirtualHost, VirtualPorts, VifPorts) ->
    Req = {pub_virt_host, PhysicalHost, VirtualHost, VirtualPorts, VifPorts},
    gen_server:call(?SERVER, Req).

publish_of_switch(VirtualHost, OFSwitch, Ports) ->
    ok.

publsh_endpoint(VirtualHost, Endpoint, VirtualPortToBound) ->
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ex_dby_lib:init(),
    {ok, Args, 0}.

handle_call(Req0, _From, State) ->
    try
        Req1 = format_req(Req0),
        validate_request(Req1),
        Reply = handle_request(Req1),
        ?INFO("Handled publish request ~p", [Req0]),
        {reply, Reply, State}
    catch
        throw:Reason ->
            ?ERROR("Failed to handle publish request ~p because ~p",
                   [Req0, Reason]),
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

format_req({pub_phy_host = ReqName, ClusterPatchPanel, PhysicalHost, Ports}) ->
    {ReqName,
     ex_dby_lib:binarize(identifier, ClusterPatchPanel),
     ex_dby_lib:binarize(identifier, PhysicalHost),
     ex_dby_lib:binarize(ports, Ports)};
format_req({pub_virt_host = ReqName, PhysicalHost, VirtualHost, VirtualPorts,
            VifPorts}) ->
    {ReqName,
     ex_dby_lib:binarize(identifier, PhysicalHost),
     ex_dby_lib:binarize(identifier, VirtualHost),
     ex_dby_lib:binarize(ports, VirtualPorts),
     ex_dby_lib:binarize(ports, VifPorts)}.

validate_request({pub_phy_host, _, PhysicalHost, _}) ->
    ex_dby_lib:identifier_exists(PhysicalHost)
        andalso throw({physical_host_exists, PhysicalHost});
validate_request({pub_virt_host, PhysicalHost, VirtualHost, VirtualPorts,
                  VifPorts}) ->
    case ex_dby_lib:identifier_is_physical_host(PhysicalHost) of
        true ->
            ok;
        not_found ->
            throw({physical_host_not_exists, PhysicalHost});
        false ->
            throw({not_physical_host, PhysicalHost})
    end,
    ex_dby_lib:identifier_exists(VirtualHost)
        andalso throw(virtual_hosts_exists),
    [begin
         VpToBound = proplists:get_value(<<"vp_to_bound">>, Props),
         proplists:is_defined(VpToBound, VirtualPorts)
             orelse throw({bad_virtual_port_to_bound, VpToBound, P})
     end|| {P, Props} <- VifPorts].

handle_request({pub_phy_host, ClusterPatchPanel, PhysicalHost, Ports}) ->
    PortsDbyEps = [ex_dby_lib:physical_port(PhysicalHost, P, Prop)
                   || {P, Prop} <- Ports],
    {PortsIdentifiers, _} = lists:unzip(PortsDbyEps),
    {PhPatchp, _} = PhPatchpDbyEp = ex_dby_lib:patch_panel(PhysicalHost,
                                                           <<"PatchP">>,
                                                           PortsIdentifiers),
    Identifiers =
        [ex_dby_lib:patch_panel(<<>>, ClusterPatchPanel, PortsIdentifiers),
         ex_dby_lib:physical_host(PhysicalHost),
         PhPatchpDbyEp
         | PortsDbyEps],
    Links1 =
        [
         [ex_dby_lib:part_of_link(Id, P) || P <- PortsIdentifiers]
         || Id <- [ClusterPatchPanel, PhysicalHost, PhPatchp]
        ],
    Links2 = [ex_dby_lib:part_of_link(PhPatchp, PhysicalHost) | Links1],
    ex_dby_lib:publish(?PUBLISHER, Identifiers ++ lists:flatten(Links2));
handle_request({pub_virt_host, PhysicalHost, VirtualHost, VirtualPorts,
                VifPorts}) ->
    {VifPortsDbyEps, {VirtPortsDbyEps, BoundToLinks}} =
        lists:mapfoldl(
          fun({VifP, VifProp}, {VirtPorts, Links}) ->
                  {_, {_, VirtP}, VifProp1} = lists:keytake(<<"vp_to_bound">>, 1, VifProp),
                  {VirtP, VirtProp} = proplists:lookup(VirtP, VirtPorts),
                  {VirtDbyId, _} = VirtDbyEp = ex_dby_lib:virtual_port(VirtualHost,
                                                                       VirtP,
                                                                       VirtProp),
                  {VifDbyId, _} = VifDbyEp = ex_dby_lib:vif_port(VirtualHost,
                                                                 VifP,
                                                                 VifProp1),
                  {VifDbyEp, {[VirtDbyEp | proplists:delete(VirtP, VirtPorts)],
                              [ex_dby_lib:bound_to_link(VirtDbyId, VifDbyId) | Links]}}
          end, {VirtualPorts, []}, VifPorts),
    VhDbyEp = {VhDbyId, _} = ex_dby_lib:virtual_host(VirtualHost),
    VhPatchpDbyEp = {VhPatchpDbyId, _} =
        ex_dby_lib:patch_panel(VhDbyId, <<"PatchP">>,
                               element(1, lists:unzip(VirtPortsDbyEps))),
    PhPatchpDbyId = ex_dby_lib:host_patch_panel(PhysicalHost),
    PhPatchpToUpdate =
        ex_dby_lib:update_patchp_wires(PhPatchpDbyId,
                                       element(1, lists:unzip(VifPortsDbyEps))),
    Identifiers = [VhDbyEp, VhPatchpDbyEp, VirtPortsDbyEps, VifPortsDbyEps,
                   PhPatchpToUpdate],
    Links1 = [
              [ex_dby_lib:part_of_link(Id, P) || {P, _} <- VirtPortsDbyEps]
              || Id <- [VhPatchpDbyId, VhDbyId]
             ],
    Links2 = [ex_dby_lib:part_of_link(VirtualHost, PhysicalHost),
              ex_dby_lib:part_of_link(VhPatchpDbyId, VirtualHost),
              [ex_dby_lib:part_of_link(PhPatchpDbyId, P)
               || {P, _} <- VifPortsDbyEps]
              | Links1 ++ BoundToLinks],
    ex_dby_lib:publish(?PUBLISHER, lists:flatten(Identifiers ++ Links2)).












