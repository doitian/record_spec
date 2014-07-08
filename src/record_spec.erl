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
