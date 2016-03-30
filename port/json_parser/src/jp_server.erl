-module(jp_server).

-behaviour(gen_server).

%% API
-export([start_link/0, parse_document/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, json_parser).
-define(PROG, "jp_prog").
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

handle_info({Port, {exit_status, Status}},
            #state{port = Port, num_exits = NExits} = State)
  when NExits < ?MAX_NUM_EXITS ->
    error_logger:format("port exited with status ~w", [Status]),
    {noreply, State#state{port = create_port(),
                          num_exits = NExits + 1}};
handle_info({Port, {exit_status, Status}},
            #state{port = Port, num_exits = NExits} = State) ->
    error_logger:format("port exited with status ~w and max num of"
                        ++ " exits (~p) was reached",
                        [Status, ?MAX_NUM_EXITS]),
    {stop, max_num_exits_reached, State#state{port = undefined,
                                              num_exits = NExits + 1}}.

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
            open_port({spawn, filename:join([Dir, ?PROG])},
                      [binary, {packet, 4}, exit_status])
    end.
