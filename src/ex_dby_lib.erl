-module(ex_dby_lib).

-export([identifier_exists/1,
         publish/2,
         physical_port/3,
         patch_panel/3,
         physical_host/1,
         part_of_link/2,
         binarize/2]).

-include_lib("dobby_clib/include/dobby.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec identifier_exists(dby_identifier()) -> boolean().

identifier_exists(Identifier) ->
    dby:search(ex_dby_funs:mk_identifier_exits(Identifier),
               not_found,
               Identifier,
               [{max_depth, 0}]) =/= not_found.

-spec publish(publisher_id(), [dby_endpoint() | link()] | dby_endpoint()) ->
                     ok | {error, reason()}.

publish(PublisherId, EndpointsOrLinks) ->
    dby:publish(PublisherId, EndpointsOrLinks, dby_default_opts()).

physical_port(Ph, Id, Properties) when is_binary(Id) ->
    {prefix(Ph, Id), [{<<"type">>, <<"lm_pp">>} | Properties]}.

patch_panel(Ph, Id, AttachedPorts) when is_binary(Id) ->
    Wires = maps:from_list([{P, null} || P <- AttachedPorts]),
    {prefix(Ph, Id), [{<<"type">>, <<"lm_patchp">>}, {<<"wires">>, Wires}]}.

physical_host(Id) when is_binary(Id) ->
    {Id, [{<<"type">>, <<"lm_ph">>}]}.

part_of_link(Src, Dst) when is_binary(Src) andalso is_binary(Dst) ->
    {Src, Dst, [{<<"type">>, <<"part_of">>}]}.

binarize(identifier, Name) ->
    list_to_binary(Name);
binarize(phy_ports, Ports) ->
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
