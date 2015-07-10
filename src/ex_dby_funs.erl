-module(ex_dby_funs).

-export([mk_identifier_exits/1]).

mk_identifier_exits(Identifier) ->
    fun(Id, _, [], _) when Id =:= Identifier->
            {stop, found};
       (_, _, _, _) ->
            {stop, not_found}
    end.
