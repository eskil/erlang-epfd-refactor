-module(db).
-export([find_user/2]).
-include("records.hrl").

find_user(_DbConn, "joe") ->
  {ok, #user_details{username="joe", hashed_password = "hash"}};
find_user(_DbConn, _NotJoe) ->
  {error, unknown_user}.
