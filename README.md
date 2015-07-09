# Executive

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc/generate-toc again -->
**Table of Contents**

- [Executive](#executive)
    - [Building and running](#building-and-running)
    - [SSH access](#ssh-access)
        - [Configuration](#configuration)
        - [Access using keys](#access-using-keys)
        - [Access via user/password](#access-via-userpassword)

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
