-module(executive_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("ex_logger.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    executive_sup:start_link().

stop(_State) ->
    ok.
