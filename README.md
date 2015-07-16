# Executive

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Executive](#executive)
    - [Building and running](#building-and-running)
    - [SSH access](#ssh-access)
        - [Configuration](#configuration)
        - [Access using keys](#access-using-keys)
        - [Access via user/password](#access-via-userpassword)
    - [Example](#example)
        - [Publishing Physical Host](#publishing-physical-host)
        - [Publishing Virtual Host](#publishing-virtual-host)
        - [Publishing OpenFlow Switch](#publishing-openflow-switch)
        - [Publishing Endpoint](#publishing-endpoint)
        - [Publishing another Physical Host and linking it to the first one](#publishing-another-physical-host-and-linking-it-to-the-first-one)

<!-- markdown-toc end -->

## Building and running

`make && _rel/executive/bin/executive`

## SSH access

### Configuration

Run `deps/erl_sshd/make_keys`. This will create:

* create a system key as `priv/erl_sshd/ssh_host_rsa_key',
* create a public and private user key in the top directory
* add the created public user key to the `priv/erl_sshd/authorized_keys`.

Adjust listen port and user/passwords in the config/sys.config file:

```erlang
 {erl_sshd, [
             {app, executive},
             {port, 11122},
             {passwords, [{"alice","secret"}]}
            ]}
```

> Remember to re-generate release after changing/creating the keys/sys.config.

Detailed information can be find in the `erl_sshd`
[README](https://github.com/marcsugiyama/erl_sshd).

### Access using keys

To access running `Executive` node via keys run in the top directory:

`ssh HOSTNAME -p 111222 -i id_rsa`

### Access via user/password

`ssh alice@HOSTNAME -p 11122`

## Example

### Publishing Physical Host

```erlang
Ports = 
   [{"PP1", #{mac_address => "00:00:00:00:01:AA", interface => "eth0"}},
    {"PP2", #{mac_address => "00:00:00:00:01:AB", interface => "eth1"}}].
executive:publish_physical_host("ClusterPatchP", "PH1", Ports).
```

### Publishing Virtual Host

It will be attached to the previously pbulished Physical Host `PH1`.

```erlang
VirtualPorts = 
   [{"VP1", [{mac_address, "00:00:00:00:01:AA"}, {interface , "eth0"}]},
    {"VP2", [{mac_address, "00:00:00:00:01:AB"}, {interface , "eth1"}]}].
VifPorts = 
   [{"VP1.1", [{vp_to_bound, "VP1"}, {interface , "vif1.1"}]},
    {"VP1.2", [{vp_to_bound, "VP2"}, {interface , "vif1.2"}]}].
executive:publish_virtual_host("PH1", "PH1/VH1", VirtualPorts, VifPorts).
```

### Publishing OpenFlow Switch

It will be attached to previously published Virtual Host `VH1`.

```erlang
Ports = 
   [{"OFP1", [{vp_to_bound, "VP1"}]},
    {"OFP2", [{vp_to_bound, "VP2"}]}].
executive:publish_of_switch("PH1/VH1", "PH1/VH1/OFS1",  Ports).
```

### Publishing Endpoint

It will be attach to the previously published Virtual Host `VH1` assuming
that the OpenFlow Switch `OFS1` was not published.

```erlang
executive:publish_endpoint("PH1/VH1", "EP1", "VP1").
```

### Publishing another Physical Host and linking it to the first one

The hosts will be connected via `PP1` Physcial Ports on each host.

```erlang
Ports = 
   [{"PP1", #{mac_address => "00:00:00:00:01:AA", interface => "eth0"}},
    {"PP2", #{mac_address => "00:00:00:00:01:AB", interface => "eth1"}}].
executive:publish_physical_host("ClusterPatchP", "PH2", Ports).
executive:bound_physical_hosts("PH1", "PP1", "PH2", "PP1").
```




