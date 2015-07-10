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
-spec publish_physical_host(string(), string(), phy_host_ports()) -> Result when
      Result :: ok |
                {error, {physical_hosts_exists, binary()} | term()}.

publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports) ->
    ex_publisher:publish_physical_host(ClusterPatchPanel, PhysicalHost, Ports).

publish_virtual_host(PhysicalHost, VirtualHost, VirtualPorts, VifPorts) ->
    ok.

publish_of_switch(VirtualHost, OFSwitch, Ports) ->
    ok.

publsh_endpoint(VirtualHost, Endpoint, VirtualPortToBound) ->
    ok.










