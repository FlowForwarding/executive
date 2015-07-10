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
%% Includes
%% ------------------------------------------------------------------

-include("executive.hrl").
-include("ex_logger.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec publish_physical_host(string(), string(), phy_host_ports()) ->
                                   ok | {error, term()}.

publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports) ->
    Req = {pub_phy_host, ClusterPatchPanel, PhysicalHost, Ports},
    gen_server:call(?SERVER, Req).

publish_virtual_host(PhysicalHost, VirtualHost, VirtualPorts, VifPorts) ->
    ok.

publish_of_switch(VirtualHost, OFSwitch, Ports) ->
    ok.

publsh_endpoint(VirtualHost, Endpoint, VirtualPortToBound) ->
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(Req0, _From, State) ->
    try
        Req1 = format_req(Req0),
        validate_request(Req1),
        Reply = handle_request(Req1),
        ?INFO("Handled publish request ~p", [Req0]),
        {reply, Reply, State}
    catch
        _:Reason ->
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
     ex_dby_lib:binarize(phy_ports, Ports)}.

validate_request({pub_phy_host, _, PhysicalHost, _}) ->
    ex_dby_lib:identifier_exists(PhysicalHost)
        andalso throw({physical_host_exists, PhysicalHost}).

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
    Links =
        [
         [ex_dby_lib:part_of_link(Id, P) || P <- PortsIdentifiers]
         || Id <- [ClusterPatchPanel, PhysicalHost, PhPatchp]
        ],
    ex_dby_lib:publish(?PUBLISHER, Identifiers ++ lists:flatten(Links)).


