-module(json).
-export([decode/1]).
-include("records.hrl").

% This simply fakes the json:decode interface.

decode(<<>>) ->
  {error, insufficient_data};
decode(<<"{\"login_request\": {\"username\": \"joe\", \"password\": \"right\"}}">>) ->
  {ok, #login_request{username="joe", password="right"}};
decode(<<"{\"login_request\": {\"username\": \"bob\", \"password\": \"right\"}}">>) ->
  {ok, #login_request{username="bob", password="right"}};
decode(<<"{\"login_request\": {\"username\": \"joe\", \"password\": \"wrong\"}}">>) ->
  {ok, #login_request{username="joe", password="wrong"}}.