-module(rebar3_elmer_compile).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, elmer).
-define(PROVIDER, compile).
-define(DEPS, [{default, app_discovery}]).
-define(ELM_MAKE_BIN, "elm-make").
-define(ELM_MAKE_OPTS, ["--yes"]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAMESPACE},      % The namespace
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 elmer compile"}, % How to use the plugin
            {opts, [ % list of options understood by the plugin
                     {elm_make, $m, "elm-make", {binary, ?ELM_MAKE_BIN},
                      "Path to elm-make binary"}
                   ]},
            {short_desc, "Compile Elm sources"},
            {desc, "Compile Elm sources"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    [begin
         %% Opts = rebar_app_info:opts(AppInfo),

         ElmFiles = case rebar_app_info:get(AppInfo, elm_files, missing) of
                        Files when is_list(Files) -> Files;
                        missing ->
                            rebar_api:abort(
                              "~s", ["Missing `{elm_files, [\"Main.elm\"]}` on rebar.config"])
                    end,

         OutDir = rebar_app_info:out_dir(AppInfo),

         Options = [
                    {elm_make, rebar_app_info:get(AppInfo, elm_make, ?ELM_MAKE_BIN)},
                    {elm_make_opts, rebar_app_info:get(AppInfo, elm_make_opts, ?ELM_MAKE_OPTS)}
                   ],

         elmer_compiler:compile(ElmFiles, {beam, OutDir}, Options)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
