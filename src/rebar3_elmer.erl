-module(rebar3_elmer).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_elmer_compile:init(State),
    {ok, State1}.
