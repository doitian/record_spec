-module(record_spec).

-type record_spec() :: tuple(). % tuple which elements are all of type `field_spec()'

-type field_type() :: atom()                                     % atom itself
                    | { Type :: atom(), Args :: [field_spec()] } % local type
                    | { Module :: atom(), Type :: atom(), Args :: [field_spec()] }. % remote type

-type field_spec() ::
        {
          Name :: atom(),                       % field name
          Pos :: integer(),                     % field pos starting from 1
          Type :: field_type()                  % field type info
        }.

-export_type([record_spec/0, field_type/0, field_spec/0]).

-export([to_list/2]).

%% @doc Convert record in the module to list.
-spec to_list(module(), record()) -> [{atom(), any()}].
to_list(Module, Record) when is_atom(Module) andalso is_tuple(Record) ->
    Specs = Module:record_spec(element(1, Record)),
    lists:map(
      fun(Pos) ->
              {Name, _, _} = element(Pos, Specs),
              {Name, element(Pos + 1, Record)}
      end,
      lists:seq(1, tuple_size(Record) - 1)
     ).
