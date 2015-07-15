-module(ex_dby_funs).

-export([mk_find_identifier/1,
         mk_get_identifier_type/1,
         mk_get_host_patchp/1,
         mk_update_patchp_wires/1]).

mk_find_identifier(Identifier) ->
    fun(Id, _, [], _) when Id =:= Identifier->
            {stop, found};
       (_, _, _, _) ->
            {stop, not_found}
    end.

mk_get_identifier_type(Identifier) ->
    fun(Id, #{<<"type">>  := #{value := Type}}, [], _)
          when Id =:= Identifier ->
            {stop, Type};
       (_, _, _, _) ->
            {stop, not_found}
    end.

mk_get_host_patchp(HostIdentifier) ->
    fun(Id, _, [], Acc) when Id =:= HostIdentifier ->
            {continue, Acc};
       (Id, #{<<"type">> := #{value := <<"lm_patchp">>}}, Path, _)
          when length(Path) =:= 1 ->
            {stop, Id};
       (_, _, _, Acc) ->
            {skip, Acc}
    end.

mk_update_patchp_wires(NewWires) ->
    fun(MdProplist) ->
            Wires0 = proplists:get_value(<<"wires">>, MdProplist),
            UpdatedWires = maps:merge(NewWires, Wires0),
            [{<<"wires">>, UpdatedWires}
             | proplists:delete(<<"wires">>, MdProplist)]
    end.




















