-module(record_spec_parse_trans_tests).

-include("record_spec.hrl").
-include("./test_helpers.hrl").

-record(user, { name :: binary(),
                age :: integer() }).

-record(group, { name :: binary(),
                 users :: [#user{}] }).

-export_record_spec([user, group]).

record_not_found_test() ->
    Forms = [
             {attribute,2,type,
              {{record,user},
               [],
               []}},
             {attribute,1,export_record_spec,[usre]}
            ],
    ?assertThrow({error, _, {record_not_found, _}}, record_spec_parse_trans:parse_transform(Forms, [])).

record_found_test() ->
    Forms = [
             {attribute,2,type,
              {{record,user},
               [],
               []}},
             {attribute,1,export_record_spec,[user]}
            ],
    record_spec_parse_trans:parse_transform(Forms, []).

record_spec_test_() ->
    UserRecordInfo = record_spec(user),
    GroupRecordInfo = record_spec(group),
    [
     ?_assertEqual(2, tuple_size(UserRecordInfo)),
     ?_assertEqual({name, 1, {union, [undefined, {binary, []}]}}, element(1, UserRecordInfo)),
     ?_assertEqual({age, 2, {union, [undefined, {integer, []}]}}, element(2, UserRecordInfo)),
     ?_assertEqual(2, tuple_size(GroupRecordInfo)),
     ?_assertEqual({name, 1, {union, [undefined, {binary, []}]}}, element(1, GroupRecordInfo)),
     ?_assertEqual({users, 2, {union, [undefined, {list, [{record, [?MODULE, user]}]}]}}, element(2, GroupRecordInfo)),
     ?_assertThrow({record_spec_not_found, [user, users]}, record_spec(user, users)),
     ?_assertThrow({record_spec_not_found, [server]}, record_spec(server))
    ].
