-type phy_ports_properties() :: #{mac_address => string(),
                                  interface => atom()}.

-type phy_host_ports() :: [{Name :: string(),
                            Properties :: phy_ports_properties()}].
