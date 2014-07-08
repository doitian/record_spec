# PJ Model

The long lost Erlang hash and model.

## Usage

Add pjm as a dependency in `rebar.config`.

    {deps, [{pjm, ".*", {git, "git://github.com/3pjgames/pjm.git"}}]}.

Add following compile option in the model module.

    -compile([{parse_transform, pjm_parse_trans}]).

See example below

```erlang
-module(user).
-compile([{parse_transform, pjm_parse_trans}]).

-pjm({fields, [{login, binary},
               {password, binary},
               {age, integer}]}).
```

The module will export method `new/0`, `new/1`, `set/2`, `set/3`, `get/2` and
`get/3`.

```
1> User = user:new().
{user, {undefined, undefined, undefined}, undefined}
2> User1 = user:set(login, <<"pjm">>, User).
{user, {<<"pjm">>, undefined, undefined}, undefined}
3> User2 = user:set([{age, 10}, {password, <<"secret">>}], User1).
{user, {<<"pjm">>, <<"secret">>, 10}, undefined}
4> User3 = user:set([{gender, male}], User2).
{user, {<<"pjm">>, <<"secret">>, 10}, [{gender, male}]}
5> user:get(login, User3).
<<"pjm">>
6> user:get([login, password], User3).
[<<"pjm">>, <<"secret">>]
7> user:get([age, {country, <<"China">>}], User3).
[10, <<"China">>]
```



