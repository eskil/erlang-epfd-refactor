-module(erlang_epfd_refactor).
-export([main/1]).
-include("records.hrl").

test_validate() ->
  {error, Error1} = validate:validate(1, #req{body = <<>>}),
  io:format("Error: ~s~n", [Error1]),
  {error, Error2} = validate:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"joe\", \"password\": \"wrong\"}}">>}),
  io:format("Error: ~s~n", [Error2]),
  {ok, Message} = validate:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"joe\", \"password\": \"right\"}}">>}),
  io:format("Success: ~s~n", [Message]),
  {error, Error3} = validate:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"bob\", \"password\": \"right\"}}">>}),
  io:format("Error: ~s~n", [Error3]).

test_validate_redux() ->
  {error, Error1} = validate_redux:validate(1, #req{body = <<>>}),
  io:format("Error: ~s~n", [Error1]),
  {error, Error2} = validate_redux:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"joe\", \"password\": \"wrong\"}}">>}),
  io:format("Error: ~s~n", [Error2]),
  {ok, Message1} = validate_redux:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"joe\", \"password\": \"right\"}}">>}),
  io:format("Success: ~s~n", [Message1]),
  {error, Error3} = validate_redux:validate(1, #req{body = <<"{\"login_request\": {\"username\": \"bob\", \"password\": \"right\"}}">>}),
  io:format("Error: ~s~n", [Error3]),
  {ok, Message2} = validate_redux:validate(1, {"joe", "right"}),
  io:format("Success: ~s~n", [Message2]),
  {error, Error4} = validate_redux:validate(1, {"bob", "right"}),
  io:format("Error: ~s~n", [Error4]).      

main(_Args) ->
  io:format("~nTesting validate~n"),
  test_validate(),
  io:format("~nTesting validate_redux~n"),
  test_validate_redux().