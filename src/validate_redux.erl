-module(validate_redux).
-export([validate/2]).
-include("records.hrl").

hash_password("right") ->
  "hash";
hash_password("wrong") ->
  "xxx".

% We publically expose two ways to validate, a #req with json, and a user/password tuple.
validate(DbConn, #req{body = ReqJsonStr}) ->
  validate_login(DbConn, json:decode(ReqJsonStr));
validate(DbConn, {Username, Password}) ->
  validate_login(DbConn, {ok, #login_request{username = Username, password = Password}}).
  
% This function is internal to the module since it uses pattern matching in a way
% that creates slightly unsightly functions (from a user perspective). Simply just
% to make it callable from validate/2.

validate_login(_DbConn, {error, Reason}) ->
  {error, Reason};
validate_login(DbConn, {ok, #login_request{username = Username, password = Password}}) ->
  validate_login(DbConn, {hash_password(Password), db:find_user(DbConn, Username)});
validate_login(_DbConn, {HashedPassword, {ok, #user_details{username = Username, hashed_password = HashedPassword}}}) ->
  % Hashed password matches the found user record hashed password
  % by the power of function pattern matching.
  {ok, "hello " ++ Username};
validate_login(_DbConn, {_HashedPassword, _UserLookupResult}) ->
  % Hashed password did not match or user not found.
  {error, "wrong name or password"}.

