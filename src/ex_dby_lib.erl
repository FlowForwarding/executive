-module(ex_dby_lib).

-export([init/0,
         identifier_exists/1,
         identifier_exists/2,
         identifier_is_physical_host/1,
         identifier_is_virtual_host/1,
         host_patch_panel/1,
         update_patchp_wires/2,
         publish/2,
         physical_port/3,
         virtual_port/3,
         vif_port/3,
         of_port/4,
         patch_panel/3,
         physical_host/1,
         virtual_host/1,
         of_switch/1,
         part_of_link/2,
         bound_to_link/2,
         binarize/2]).

-include_lib("dobby_clib/include/dobby.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

init() ->
    {module, M} = code:ensure_loaded(M = ex_dby_funs),
    ok = global:sync(),
    {module, M} = dby:install(M).

-spec identifier_exists(dby_identifier()) -> boolean().

identifier_exists(Identifier) ->
    dby:search(ex_dby_funs:mk_find_identifier(Identifier),
               ignored,
               Identifier,
               [{max_depth, 0}]) =:= found.

identifier_exists(PrefixIdentifeir, Identifier) ->
    IdToFind = prefix(PrefixIdentifeir, Identifier),
    dby:search(ex_dby_funs:mk_find_identifier(IdToFind),
               ignored,
               IdToFind,
               [{max_depth, 0}]) =:= found.


-spec identifier_is_physical_host(dby_identifier()) -> boolean() | not_found.

identifier_is_physical_host(Identifier) ->
    Type = dby:search(ex_dby_funs:mk_get_identifier_type(Identifier),
                      ignored,
                      Identifier,
                      [{max_depth, 0}]),
    case Type of
        not_found ->
            not_found;
        Other when is_binary(Other)->
            Other =:= <<"lm_ph">>
    end.


-spec identifier_is_virtual_host(dby_identifier()) -> boolean() | not_found.

identifier_is_virtual_host(Identifier) ->
    Type = dby:search(ex_dby_funs:mk_get_identifier_type(Identifier),
                      ignored,
                      Identifier,
                      [{max_depth, 0}]),
    case Type of
        not_found ->
            not_found;
        Other when is_binary(Other)->
            Other =:= <<"lm_vh">>
    end.


-spec host_patch_panel(dby_identifier()) -> dby_identifier()
                                                          | not_found.

host_patch_panel(PhIdentifier) ->
    dby:search(ex_dby_funs:mk_get_host_patchp(PhIdentifier),
               not_found,
               PhIdentifier,
               [{max_depth, 2}]).


-spec update_patchp_wires(dby_identifier(), [dby_identifier()]) -> Result when
      Result :: {dby_identifier(),
                 fun((metadata_proplist()) -> metadata_proplist())}.

update_patchp_wires(PatchpId, NewAttachedPorts) ->
    NewWires = maps:from_list([{P, null} || P <- NewAttachedPorts]),
    {PatchpId, ex_dby_funs:mk_update_patchp_wires(NewWires)}.


-spec publish(publisher_id(), [dby_endpoint() | link()] | dby_endpoint()) ->
                     ok | {error, reason()}.

publish(PublisherId, EndpointsOrLinks) ->
    dby:publish(PublisherId, EndpointsOrLinks, dby_default_opts()).

physical_port(Ph, Id, Properties) when is_binary(Id) ->
    {prefix(Ph, Id), [{<<"type">>, <<"lm_pp">>} | Properties]}.

virtual_port(Vh, Id, Properties) when is_binary(Id) ->
    {prefix(Vh, Id), [{<<"type">>, <<"lm_vp">>} | Properties]}.

vif_port(Vh, Id, Properties) when is_binary(Id) ->
    {prefix(Vh, Id), [{<<"type">>, <<"lm_vp">>} | Properties]}.

of_port(Ofs, Vh, Id, Properties0) when is_binary(Id) ->
    Vp = proplists:get_value(K = <<"vp_to_bound">>, Properties0),
    Properties1 = [{K, prefix(Vh, Vp)} | proplists:delete(K, Properties0)],
    {prefix(Ofs, Id), [{<<"type">>, <<"lm_of_port">>} | Properties1]}.

patch_panel(Host, Id, AttachedPorts) when is_binary(Id) ->
    Wires = maps:from_list([{P, null} || P <- AttachedPorts]),
    {prefix(Host, Id), [{<<"type">>, <<"lm_patchp">>}, {<<"wires">>, Wires}]}.

physical_host(Id) when is_binary(Id) ->
    {Id, [{<<"type">>, <<"lm_ph">>}]}.

virtual_host(Id) when is_binary(Id) ->
    {Id, [{<<"type">>, <<"lm_vh">>}]}.

of_switch(Id) when is_binary(Id) ->
    {Id, [{<<"type">>, <<"lm_of_switch">>}]}.

part_of_link(Src, Dst) when is_binary(Src) andalso is_binary(Dst) ->
    {Src, Dst, [{<<"type">>, <<"part_of">>}]}.

bound_to_link(Src, Dst) when is_binary(Src) andalso is_binary(Dst) ->
    {Src, Dst, [{<<"type">>, <<"bound_to">>}]}.

binarize(identifier, Name) ->
    list_to_binary(Name);
binarize(ports, Ports) ->
    [{list_to_binary(Name), binarize_properties(Properties)}
     || {Name, Properties} <- Ports].


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dby_default_opts() ->
    [persistent].

prefix(<<>>, Name) ->
    <<Name/binary>>;
prefix(Prefix, Name) ->
    <<Prefix/binary, "/", Name/binary>>.

binarize_properties(Properties) ->
    [{atom_to_binary(K, utf8), binarize_property_value(V)}
     || {K,V} <- maps:to_list(Properties)].

binarize_property_value(Value) when is_list(Value) ->
    list_to_binary(Value);
binarize_property_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8).
