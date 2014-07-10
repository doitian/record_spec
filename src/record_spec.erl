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
-export([get_value/3, get_value/4, set_value/4]).

%% @doc Convert record in the module to list.
-spec to_list(module(), record()) -> [{atom(), any()}].
to_list(Module, Record) when is_atom(Module) andalso is_tuple(Record) ->
    Specs = Module:record_spec(element(1, Record)),
    lists:map(
      fun(Pos) ->
              {Name, _, _} = element(Pos, Specs),
              {Name, element(Pos + 1, Record)}
      end,
      lists:seq(1, tuple_size(Specs))
     ).

%% @doc Get record field value by name. Throws exception when Key not found.
-spec get_value(module(), atom(), record()) -> term() | no_return().
get_value(Module, Key, Record) ->
    {_, Pos, _} = Module:record_spec(element(1, Record), Key),
    element(Pos + 1, Record).

%% @doc Get record field value by name with default value. Return default
%% value even if the Key not found.
-spec get_value(module(), atom(), term(), record()) -> term().
get_value(Module, Key, Default, Record) ->
    case Module:find_record_spec(element(1, Record), Key) of
        {ok, {_, Pos, _}} ->
            case element(Pos + 1, Record) of
                undefined -> Default;
                Result -> Result
            end;
        _ -> Default
    end.

%% @doc Set record value by field name.
-spec set_value(module(), atom(), term(), record()) -> record().
set_value(Module, Key, Value, Record) ->
    {_, Pos, _} = Module:record_spec(element(1, Record), Key),
    setelement(Pos + 1, Record, Value).
