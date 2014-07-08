%%%-------------------------------------------------------------------
%%% @doc Generate record_spec/1 and record_spec/2
%%%
%%% Export record_spec to be used in runtime.
%%%
%%% Example, following code
%%%
%%%     -record(user, { name = <<>> :: binary(),
%%%                     age = 0 :: integer()
%%%                   }).
%%%     
%%%     -record(group, { name = <<>> :: binary(),
%%%                      users = [] :: [#user{}]
%%%                    }).
%%%     
%%%     -export_record_spec([user, group])
%%%
%%% when complied with this parse transform, will generate following functions
%%%
%%%    -export([record_spec/1, record_spec/2]).
%%%    record_spec(user) ->
%%%        {
%%%          {name, 1, {binary, []}},
%%%          {age, 2, {integer, []}}
%%%        };
%%%    record_spec(group) ->
%%%        {
%%%          {name, 1, {binary, []}},
%%%          {users, 2, {list, [{record, [user]}]}}
%%%        }.
%%%    
%%%    record_spec(user, name) -> {name, 1, binary};
%%%    record_spec(user, age) -> {age, 2, integer};
%%%    record_spec(group, name) -> {name, 1, binary};
%%%    record_spec(group, users) -> {users, 2, {list, [{record, user}]}}.
%%% @end
%%%-------------------------------------------------------------------
-module(record_spec_parse_trans).

-export([parse_transform/2, format_error/1]).

-record(context,
        {
          records = dict:new(),
          export_records = [],
          last_export_form
        }
       ).

parse_transform(Forms, _Options) ->
    Ctx = lists:foldl(
            fun({attribute, _, export_record_spec, Records} = Form,
                #context{ export_records = OldRecords } = CtxAcc) ->
                    CtxAcc#context{ export_records = [Records | OldRecords],
                                    last_export_form = Form };
               ({attribute, _, type, {{record, Record}, TypeSpec, _}},
                #context{ records = Dict } = CtxAcc) ->
                    CtxAcc#context{ records = dict:store(Record, TypeSpec, Dict) };
               (_, CtxAcc) -> CtxAcc
            end,
            #context{},
            Forms
           ),
    LastForm = Ctx#context.last_export_form,
    AllRecordDict = Ctx#context.records,

    ExportDict = lists:foldl(
                   fun(Record, Dict) ->
                           case dict:find(Record, AllRecordDict) of
                               {ok, Value} -> dict:store(Record, Value, Dict);
                               error ->
                                   throw({error, ?LINE, {record_not_found, erlang:get_stacktrace()}})
                           end
                   end,
                   dict:new(),
                   lists:flatten(Ctx#context.export_records)
                  ),
    lists:append(
      lists:map(
        fun({attribute, Line, export_record_spec, _} = Form) when Form =:= LastForm ->
                [ Form | generate_fun(Line, ExportDict) ];
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

generate_fun(Line, ExportDict) ->
    Export = {attribute,Line,export,[{record_spec,1},{record_spec,2}]},
    {RecordSpec1Clauses, DeepRecordSpec2Clauses} =
        dict:fold(
          fun(Record, Types, {Forms1, Forms2}) ->
                  TypesSpec = generate_types(Line, Types),
                  Clause1 =
                      {clause,Line,
                       [{atom,Line,Record}],
                       [],
                       [{tuple,Line,TypesSpec}]},
                  Clauses2 =
                      lists:map(
                        fun({tuple,_,[{atom,_,Field}|_]} = TypeSpec) ->
                                {clause,Line,
                                 [{atom,Line,Record},{atom,Line,Field}],
                                 [],
                                 [TypeSpec]}
                        end,
                        TypesSpec
                       ),
                  {[Clause1|Forms1], [Clauses2|Forms2]}
          end,
          {[], []},
          ExportDict
         ),
    RecordSpec1 = {function,Line,record_spec,1,RecordSpec1Clauses},
    RecordSpec2 = {function,Line,record_spec,2,lists:append(DeepRecordSpec2Clauses)},

    [Export, RecordSpec1, RecordSpec2].

generate_type(Line, Pos, {typed_record_field, RecordField, TypeSpec}) ->
    {tuple,Line,[{atom,Line,get_record_field_name(RecordField)},
                 {integer,Line,Pos},generate_type(Line, TypeSpec)]};
generate_type(Line, Pos, RecordField) ->
    {tuple,Line,[{atom,Line,get_record_field_name(RecordField)},
                 {integer,Line,Pos},
                 {atom,Line,any}]}.

generate_types(Line, Types) ->
    lists:zipwith(
      fun(T, Pos) -> generate_type(Line, Pos, T) end,
      Types,
      lists:seq(1, length(Types))
     ).

get_record_field_name(RecordField) ->
    {atom , _, Name} = element(3, RecordField),
    Name.

generate_type(Line, {atom, _, Atom}) ->
    {atom, Line, Atom};
generate_type(Line, {type, _, Type, Types}) when is_list(Types) ->
    { tuple, Line,
      [
       {atom, Line, Type},
       generate_type_map(Line, Types)
      ]
    };
generate_type(Line, {remote_type, _, [{atom, _, M}, {atom, _, F}, A]}) ->
    { tuple, Line,
      [
       {atom, Line, M},
       {atom, Line, F},
       generate_type_map(Line, A)
      ]
    }.

generate_type_map(Line, Types) ->
    lists:foldr(
      fun(Type, Cons) ->
              { cons,
                Line,
                generate_type(Line, Type),
                Cons }
      end,
      {nil, Line},
      Types
     ).
