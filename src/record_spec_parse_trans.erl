%%%-------------------------------------------------------------------
%%% @doc Generate  `find_record_spec/1', `find_record_spec/2', `record_spec/0', `record_spec/1' and `record_spec/2'.
%%%
%%% Export records information so they can be used in runtime.
%%%
%%% Example, following code
%%%
%%% ```
%%%     -record(user, { name = <<>> :: binary(),
%%%                     age = 0 :: integer()
%%%                   }).
%%%     
%%%     -record(group, { name = <<>> :: binary(),
%%%                      users = [] :: [#user{}]
%%%                    }).
%%%     
%%%     -export_record_spec([user, group])
%%% '''
%%%
%%% when complied with this parse transform, will generate following functions
%%%
%%% ```
%%%    -export([find_record_spec/1, find_record_spec/2, record_spec/0, record_spec/1, record_spec/2]).
%%%    -spec find_record_spec(atom()) -> {ok, record_spec:record_spec()} | error.
%%%    find_record_spec(user) ->
%%%        {
%%%          ok,
%%%          {
%%%            {name, 1, {binary, []}},
%%%            {age, 2, {integer, []}}
%%%          }
%%%        };
%%%    find_record_spec(group) ->
%%%        {
%%%          ok,
%%%          {
%%%            {name, 1, {binary, []}},
%%%            {users, 2, {list, [{record, [user]}]}}
%%%          }
%%%        };
%%%    find_record_spec(_) -> error.
%%%    
%%%    -spec find_record_spec(atom(), atom()) -> {ok, record_spec:field_spec()} | error.
%%%    find_record_spec(user, name) -> {ok, {name, 1, binary}};
%%%    find_record_spec(user, age) -> {ok, {age, 2, integer}};
%%%    find_record_spec(group, name) -> {ok, {name, 1, binary}};
%%%    find_record_spec(group, users) -> {ok, {users, 2, {list, [{record, user}]}}};
%%%    find_record_spec(_, _) -> error.
%%%
%%%    -spec record_spec() -> [atom()].
%%%    record_spec() ->
%%%        [user, group].
%%%
%%%    -spec record_spec(atom()) -> record_spec:record_spec().
%%%    record_spec(Record) ->
%%%        case find_record_spec(Record) of
%%%            {ok, Value} -> Value;
%%%            error -> throw({record_spec_not_found, [Record]})
%%%        end.
%%%    -spec record_spec(atom(), atom()) -> record_spec:field_spec().
%%%    record_spec(Record, Field) ->
%%%        case find_record_spec(Record, Field) of
%%%            {ok, Value} -> Value;
%%%            error -> throw({record_spec_not_found, [Record, Field]})
%%%    end.
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(record_spec_parse_trans).

-export([parse_transform/2, format_error/1]).

-record(context,
        {
          records = dict:new(),
          export_records = [],
          last_export_form,
          module
        }
       ).

parse_transform(Forms, _Options) ->
    AllRef = make_ref(),
    Ctx = lists:foldl(
            fun({attribute, _, module, Module}, CtxAcc) ->
                    CtxAcc#context{ module = Module };
               ({attribute, _, export_record_spec, Records} = Form,
                #context{ export_records = OldRecords } = CtxAcc) ->
                    NewRecords = [
                                  case Records of
                                      all -> [AllRef];
                                      _ -> Records
                                  end
                                  | OldRecords
                                 ],
                    CtxAcc#context{ export_records = NewRecords,
                                    last_export_form = Form };
               ({attribute, _, type, {{record, Record}, TypeSpec, _}},
                #context{ records = Dict } = CtxAcc) ->
                    CtxAcc#context{ records = dict:store(Record, TypeSpec, Dict) };
               (_, CtxAcc) -> CtxAcc
            end,
            #context{},
            Forms
           ),
    Module = Ctx#context.module,
    LastForm = Ctx#context.last_export_form,
    AllRecordDict = Ctx#context.records,
    ExportRecords0 = lists:flatten(Ctx#context.export_records),
    ExportRecords = case lists:member(AllRef, ExportRecords0) of
                        true -> dict:fetch_keys(AllRecordDict);
                        false -> ExportRecords0
                    end,

    ExportDict = lists:foldl(
                   fun(Record, Dict) ->
                           case dict:find(Record, AllRecordDict) of
                               {ok, Value} -> dict:store(Record, Value, Dict);
                               error ->
                                   throw({error, ?LINE, {record_not_found, erlang:get_stacktrace()}})
                           end
                   end,
                   dict:new(),
                   ExportRecords
                  ),
    lists:append(
      lists:map(
        fun({attribute, Line, export_record_spec, _} = Form) when Form =:= LastForm ->
                [ Form | generate_fun(Module, Line, ExportDict) ];
           (Form) ->
                [Form]
        end,
        Forms
       )
     ).

format_error(E) ->
    case io_lib:deep_char_list(E) of
        true ->
            E;
        _ ->
            io_lib:write(E)
    end.

%%% ==================================================================
%%% Private Functions
%%% ==================================================================

generate_fun(Module, Line, ExportDict) ->
    Export = {attribute,Line,export,[{find_record_spec,1},{find_record_spec,2},{record_spec,0},{record_spec,1},{record_spec,2}]},

    {FindRecordSpec1Clauses, DeepFindRecordSpec2Clauses} =
        dict:fold(
          fun(Record, Types, {Forms1, Forms2}) ->
                  TypesSpec = generate_types(Module, Line, Types),
                  Clause1 =
                      {clause,Line,
                       [{atom,Line,Record}],
                       [],
                       [{tuple,Line,
                         [{atom,Line,ok},
                          {tuple,Line,TypesSpec}]}]},
                  Clauses2 =
                      lists:map(
                        fun({tuple,_,[{atom,_,Field}|_]} = TypeSpec) ->
                                {clause,Line,
                                 [{atom,Line,Record},{atom,Line,Field}],
                                 [],
                                 [{tuple,Line,
                                   [{atom,Line,ok},
                                    TypeSpec]}]}
                        end,
                        TypesSpec
                       ),
                  {[Clause1|Forms1], [Clauses2|Forms2]}
          end,
          {[], []},
          ExportDict
         ),
    FindRecordSpec1 = {function,Line,find_record_spec,1,
                       lists:reverse(
                         [
                          {clause,Line,
                           [{var,Line,'_'}],
                           [],
                           [{atom,Line,error}]
                          }
                          | FindRecordSpec1Clauses
                         ]
                        )
                  },
    FindRecordSpec2 = {function,Line,find_record_spec,2,
                   lists:reverse(
                     [
                      {clause,Line,
                       [{var,Line,'_'},{var,Line,'_'}],
                       [],
                       [{atom,Line,error}]
                      }
                      | lists:append(DeepFindRecordSpec2Clauses)
                     ]
                    )
                  },
    RecordSpec0 = {function,Line,record_spec,0,
                   [{clause,Line,
                     [],
                     [],
                     [
                      lists:foldr(
                        fun(Record, Cons) ->
                                { cons,
                                  Line,
                                  {atom, Line, Record},
                                  Cons
                                }
                        end,
                        {nil, Line},
                        dict:fetch_keys(ExportDict)
                       )
                     ]}]},
    RecordSpec1 = {function,Line,record_spec,1,
                   [{clause,Line,
                     [{var,Line,'Record'}],
                     [],
                     [{'case',Line,
                       {call,Line,
                        {atom,Line,find_record_spec},
                        [{var,Line,'Record'}]},
                       [{clause,Line,
                         [{tuple,Line,[{atom,Line,ok},{var,Line,'Value'}]}],
                         [],
                         [{var,Line,'Value'}]},
                        {clause,Line,
                         [{atom,Line,error}],
                         [],
                         [{call,Line,
                           {atom,Line,throw},
                           [{tuple,Line,
                             [{atom,Line,record_spec_not_found},
                              {cons,Line,
                               {var,Line,'Record'},
                               {nil,Line}}]
                            }]}]}]}]}]},
    RecordSpec2 = {function,Line,record_spec,2,
                   [{clause,Line,
                     [{var,Line,'Record'},{var,Line,'Field'}],
                     [],
                     [{'case',Line,
                       {call,Line,
                        {atom,Line,find_record_spec},
                        [{var,Line,'Record'},{var,Line,'Field'}]},
                       [{clause,Line,
                         [{tuple,Line,[{atom,Line,ok},{var,Line,'Value'}]}],
                         [],
                         [{var,Line,'Value'}]},
                        {clause,Line,
                         [{atom,Line,error}],
                         [],
                         [{call,Line,
                           {atom,Line,throw},
                           [{tuple,Line,
                             [{atom,Line,record_spec_not_found},
                              {cons,Line,
                               {var,Line,'Record'},
                               {cons,Line,{var,Line,'Field'},{nil,Line}}}]}]}]}]}]}]},
    %% io:format("~p", [[Export, FindRecordSpec1, FindRecordSpec2, RecordSpec1, RecordSpec2]]),
    [Export, FindRecordSpec1, FindRecordSpec2, RecordSpec0, RecordSpec1, RecordSpec2].

generate_type(Module, Line, Pos, {typed_record_field, RecordField, TypeSpec}) ->
    {tuple,Line,[{atom,Line,get_record_field_name(RecordField)},
                 {integer,Line,Pos},generate_type(Module, Line, TypeSpec)]};
generate_type(_Module, Line, Pos, RecordField) ->
    {tuple,Line,[{atom,Line,get_record_field_name(RecordField)},
                 {integer,Line,Pos},
                 {atom,Line,any}]}.

generate_types(Module, Line, Types) ->
    lists:zipwith(
      fun(T, Pos) -> generate_type(Module, Line, Pos, T) end,
      Types,
      lists:seq(1, length(Types))
     ).

get_record_field_name(RecordField) ->
    {atom , _, Name} = element(3, RecordField),
    Name.

generate_type(_Module, Line, {atom, _, Atom}) ->
    {atom, Line, Atom};
generate_type(Module, Line, {type, _, record, [{atom, _, Record}]}) ->
    { tuple, Line,
      [
       {atom, Line, record},
       {cons, Line,
        {atom, Line, Module},
        {cons, Line,
         {atom, Line, Record},
         {nil, Line}
        }
       }
      ]
    };
generate_type(Module, Line, {type, _, Type, Types}) when is_list(Types) ->
    { tuple, Line,
      [
       {atom, Line, Type},
       generate_type_map(Module, Line, Types)
      ]
    };
generate_type(Module, Line, {remote_type, _, [{atom, _, M}, {atom, _, F}, A]}) ->
    { tuple, Line,
      [
       {atom, Line, M},
       {atom, Line, F},
       generate_type_map(Module, Line, A)
      ]
    }.

generate_type_map(Module, Line, Types) ->
    lists:foldr(
      fun(Type, Cons) ->
              { cons,
                Line,
                generate_type(Module, Line, Type),
                Cons }
      end,
      {nil, Line},
      Types
     ).
