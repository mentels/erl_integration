-module(json_parser).

-export([start/0, parse/1]).

start() ->
    application:ensure_started(json_parser).

parse(Data) ->
    jp_server:parse_document(Data).
