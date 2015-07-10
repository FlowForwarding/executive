-module(executive_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_DOBBY_NODE, 'dobby@127.0.0.1').

-include("ex_logger.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    try
        connect_to_dobby(),
        executive_sup:start_link()
    catch
        throw:Reason ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

connect_to_dobby() ->
    DobbyNode = application:get_env(executive, dobby_node, ?DEFAULT_DOBBY_NODE),
    case net_adm:ping(DobbyNode) of
        pong ->
            ?INFO("Connected to dobby node: ~p", [DobbyNode]);
        pang ->
            ?ERROR("Failed to connect to dobby node: ~p", [DobbyNode]),
            throw({connecting_to_dobby_failed, DobbyNode})
    end.
