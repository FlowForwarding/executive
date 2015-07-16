-module(executive).

-export([publish_physical_host/3,
         publish_virtual_host/4,
         publish_of_switch/3,
         publsh_endpoint/3]).

-include("executive.hrl").
-include("ex_logger.hrl").

%% @doc This publisher creates a structure for `PhysicalHost' in Dobby.
%%
%% It creates the following identifiers:
%% 1) Physical Host
%% 2) Patch Panel of PH
%% 3) Physical Ports of PH
%%
%% The publisher connects all the above using ‘part_of’ links. If the
%% `ClusterPatchPanel' doesn’t exists it is created by the publisher.
%% All the identifiers created by the publisher are prefixed
%% with the `PhysicalHost'.
-spec publish_physical_host(string(), string(), phy_ports()) -> Result when
      Result :: ok |
                {error, {physical_hosts_exists, PhysicalHost :: binary()}
                 | term()}.

publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports) ->
    ex_publisher:publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports).


%% @doc The publisher creates a structure for a Virtual Host in Dobby.
%%
%% The publisher links the new VH to an existing PH. It creates the
%% following identifiers:
%% 1) Virtual Host (VH)
%% 2) Patch Panel of VH (PatchP)
%% 3) Virtual Ports of VH (VP)
%% 4) Virtual Ports of PH (VIF)
%% All the identifiers created by the VH publisher are prefixed
%% with the Virtual Host. The publisher also creates appropriate links:
%% 1) ‘bound_to’ ones for the VPs and VIFs,
%% 2)‘part_of’ for the rest.
%% In the future, creating Virtual Ports of PH (Vifs) will be the
%% responsibility of the Physical Host Publisher/Subscriber. Upon detecting
%% a new VH starting the publisher will create appropriate Vifs identifiers’ i
%% n Dobby and attach them to the Patch Panel of PH.
-spec publish_virtual_host(string(), string(), virt_ports(), vif_ports()) ->
                                  Result when
      Result :: ok |
                {error, Reason},
      Reason :: {physical_host_not_exists, PhysicalHost :: binary()} |
                {not_physical_host, PhysicalHost :: binary()} |
                {virtual_host_exists, VirtualHost :: binary()} |
                {bad_virtual_port_to_bound, VirtualPortToBound :: binary(),
                 VifPort :: binary()} |
                term().

publish_virtual_host(PhysicalHost, VirtualHost, VirtualPorts, VifPorts) ->
    ex_publisher:publish_virtual_host(
      PhysicalHost, VirtualHost, VirtualPorts, VifPorts).


%% @doc The publisher creates a structure for an OpenFow Switch in Dobby.
%%
%% It links the OFS to an existing Virtual Host. It creates the following
%% identifiers:
%% 1) OpenFlow Switch
%% 2)OpenFlow Port
%% All the identifiers created by the OFS publisher are prefixed
%% with the OpenFlow Switch.  The publisher also creates appropriate links.
-spec publish_of_switch(string(), string(), of_ports()) -> Result when
      Result :: ok |
                {error, Reason},
      Reason :: {virtual_host_not_exists, VirtualHost :: binary()} |
                {not_virtual_host, VirtualHost :: binary()} |
                {virtual_port_to_bound_not_exists,
                 VirtualPortToBound :: binary(), OfPort :: binary()} |
                term().

publish_of_switch(VirtualHost, OfSwitch, Ports) ->
    ex_publisher:publish_of_switch(VirtualHost, OfSwitch, Ports).


publsh_endpoint(VirtualHost, Endpoint, VirtualPortToBound) ->
    ok.













