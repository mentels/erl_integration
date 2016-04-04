-module(jp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, parse_document/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, json_parser).
-define(DRIVER, "jp_driver").
-define(MAX_NUM_EXITS, 5).

-record(state, {port :: port(), num_exits = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

parse_document(Doc) when is_binary(Doc) ->
    gen_server:call(?SERVER, {parse_document, Doc}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{port = create_port()}}.


handle_call({parse_document, Doc}, _From,
            #state{port = Port} = State) ->
    %% standard external transport format
    Port ! {self(), {command, term_to_binary(Doc)}},
    receive
        {Port, {data, EncDoc}} ->
            {reply, binary_to_term(EncDoc), State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_port() ->
    case code:priv_dir(?APP) of
        {error, Reason} ->
            ErrorS = io_lib:format("~w priv dir not found: ~w",
                                   [?APP, Reason]),
            error_logger:format("~s", [ErrorS]),
            error(list_to_atom(lists:flatten(ErrorS)));
        Dir ->
            ok = load_driver(Dir, ?DRIVER),
            open_port({spawn, ?DRIVER}, [binary])
    end.

load_driver(Path, Name) ->
    case erl_ddll:load(Path, Name) of
        ok ->
            ok;
        {error, ErrorDesc} ->
            ErrorDescS = erl_ddll:format_error(ErrorDesc),
            ErrorS = io_lib:format("Error loading driver ~p from ~p because: ~s",
                                   [Path, Name, ErrorDescS]),
            error_logger:format("~s", [ErrorS]),
            exit({error_loading_dirver, Name, ErrorDescS})
    end.
