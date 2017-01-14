# What

Example of a refactor of the `validate/2` function described in [Erlang encourages poor functional design](https://srparish.wordpress.com/2016/10/17/erlang-encourages-poor-functional-design/).

# The original

The article shows this snippet as an example of poor style.

```erlang
validate(_DbConn, req#{body = <<>>}) ->
  {:error, "No body provided"};
validate(DbConn, req#{body = ReqJsonStr}) ->
  case json:decode(ReqJsonStr) of
    {:error, Reason} -> {:error, Reason};
    {:ok, #login_request{username = Username, password = Password}} ->
      case db.find_user(DbConn, Username) of
        {:error, Reason} -> {:error, Reason};
        {:ok, #user_details{hashed_password = HashedPassword}} ->
          ...
      end
  end.
```

The author proposes this refactor, and uses it as an example of erlang encouraging poor functional design.

```erlang
validate(_DbConn, req#{body = <<>>}) ->
  {:error, "No body provided"};
validate(DbConn, req#{body = ReqJsonStr}) ->
  decode_json(DbConn, ReqJsonStr).

decode_json(DbConn, ReqJsonStr) ->
  case json:decode(ReqJsonStr) of
    {:error, Reason} ->  {:error, Reason};
    {:ok, #login_request{username = ReqUsername, password = ReqPassword}}) ->
      find_user(DbConn, ReqUsername, ReqPassword)
  end.

find_user(DbConn, ReqUsername, ReqPassword) ->
  case db.find_user(DbConn, ReqUsername, ReqPassword) of
    {:error, Reason} -> {:error, Reason};
    {:ok, #user_details{hashed_password = HashedPassword}} ->
      check_password(ReqUsername, ReqPassword, HashedPassword)
  end.

...
```

# Refactored

See `src/validate.erl`.

```erlang
validate(_DbConn, {error, Reason}) ->
  {error, Reason};
validate(DbConn, #req{body = ReqJsonStr}) ->
  validate(DbConn, json:decode(ReqJsonStr));
validate(DbConn, {ok, #login_request{username = Username, password = Password}}) ->
  validate(DbConn, {hash_password(Password), db:find_user(DbConn, Username)});
validate(_DbConn,
         {HashedPassword,
          {ok, #user_details{username = Username, hashed_password = HashedPassword}}}) ->
  % Hashed password matches the found user record hashed password
  % by the power of function pattern matching.
  {ok, "hello " ++ Username};
validate(_DbConn, {_HashedPassword, _UserLookupResult}) ->
  % Hashed password did not match or user not found.
  {error, "wrong name or password"}.
```	

This fixes the concerns in the article;

 1. We're not passing a db conn to a json decode function.
 2. json decoder isn't calling `db:find_user`.
 3. We're passing aroung the raw password less.
 4. Each function is decoupled and easily testable.

This still smells bad. It's odd for `validate/2` to
take arguments that are tuples ala `{ok, #login_request}`. If someone wanted
to login without using json, they would be tempted to directly call

```erlang
validate(..., {ok, #login_request{username = Username, password = Password}})
```

which is awkward.

Splitting it into an exported `validate/2` and an non-exported set of
functions (private) would be better. See `src/validate_redux.erl` for full file.

```erlang
% Public

validate(DbConn, #req{body = ReqJsonStr}) ->
  validate_login(DbConn, json:decode(ReqJsonStr));
validate(DbConn, {Username, Password}) ->
  validate_login(DbConn, {ok, #login_request{username = Username, password = Password}}).
  
% Private

validate_login(_DbConn, {error, Reason}) ->
  {error, Reason};
validate_login(DbConn, {ok, #login_request{username = Username, password = Password}}) ->
  validate_login(DbConn, {hash_password(Password), db:find_user(DbConn, Username)});
validate_login(_DbConn,
               {HashedPassword,
                {ok, #user_details{username = Username, hashed_password = HashedPassword}}}) ->
  % Hashed password matches the found user record hashed password
  % by the power of function pattern matching.
  {ok, "hello " ++ Username};
validate_login(_DbConn, {_HashedPassword, _UserLookupResult}) ->
  % Hashed password did not match or user not found.
  {error, "wrong name or password"}.
```


# To run

    make
    ./erlang_epfd_refactor
    
# Assumptions

The original article seems to assume a `json:decode/1` function that
parses into records, so `json.erl` does that. Additionally, it also
returns `{error, insufficient_data}` on empty input, which isn't unreasonable. See eg. [this](https://github.com/hio/erlang-json/blob/9800e234102d070733f28d1d4b10126548d90555/c_src/decode.c#L875) common erlang json library.

If not, we'd have to add one extra `validate/2` function that handles the empty body case. 

Further, it relies on functions returning `{ok, ...}` and `{error,
Message}` in general which is not unreasonable.

# Conclusion

It's easy to write tightly coupled and bad code in any language. 
It's easy to refactor it to be less bad but not still what would 
objectively be "good".

While there's plenty of examples of "bad" erlang code, specifically
over-use of `case` in erlang, I would not agree that erlang encourages
poor functional design.

And yes, proper exunit tests for this would've been nice.
And no, I do not claim that this is the best refactor.
