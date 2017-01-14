-module(validate).
-export([validate/2]).
-include("records.hrl").

hash_password("right") ->
  "hash";
hash_password("wrong") ->
  "xxx".

validate(_DbConn, {error, Reason}) ->
  {error, Reason};
validate(DbConn, #req{body = ReqJsonStr}) ->
  validate(DbConn, json:decode(ReqJsonStr));
validate(DbConn, {ok, #login_request{username = Username, password = Password}}) ->
  validate(DbConn, {hash_password(Password), db:find_user(DbConn, Username)});
validate(_DbConn, {HashedPassword, {ok, #user_details{username = Username, hashed_password = HashedPassword}}}) ->
  % Hashed password matches the found user record hashed password
  % by the power of function pattern matching.
  {ok, "hello " ++ Username};
validate(_DbConn, {_HashedPassword, _UserLookupResult}) ->
  % Hashed password did not match or user not found.
  {error, "wrong name or password"}.

