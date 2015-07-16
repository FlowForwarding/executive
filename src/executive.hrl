%% ports properties

-type phy_ports_properties() :: #{mac_address => string(),
                                  interface => atom()}.

-type virt_ports_properties() :: phy_ports_properties().

-type vif_ports_properties() :: #{vp_to_bound => string(),
                                  mac_address => string(),
                                  interface => atom()}.

-type of_ports_properties() :: #{vp_to_bound => string()}.

%% ports

-type phy_ports() :: [{Name :: string(),
                            Properties :: phy_ports_properties()}].

-type virt_ports() :: [{Name :: string(),
                             Properties :: virt_ports_properties()}].

-type vif_ports() :: [{Name :: string(),
                       Properties :: vif_ports_properties()}].

-type of_ports() :: [{Name :: string,
                     Propertes :: of_ports_properties()}].
