# Record Spec

Generate record info for runtime.

## Usage

Add `record_spec` as a dependency in `rebar.config`.

    {deps, [{record_spec, ".*", {git, "git://github.com/3pjgames/record_spec.git"}}]}.

Add following code to export records, e.g., `user` and `group`.

    -include("record_spec/include/record_spec.hrl").
    -export_record_spec([user, group]).

See example below

```erlang
-module(models).
-include("record_spec/include/record_spec.hrl").
-record(user, { name :: binary(),
                age :: integer() }).

-record(group, { name :: binary(),
                 users :: [#user{}] }).

-export_record_spec([user, group]).
```
