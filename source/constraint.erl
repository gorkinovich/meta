%%%|========================================================================================|
%%%| META: Mini-Erlang Typing Application                                                   |
%%%| Copyright (C) 2019-2021, Gorka Suárez García                                           |
%%%|                                                                                        |
%%%| This program is free software: you can redistribute it and/or modify                   |
%%%| it under the terms of the GNU General Public License as published by                   |
%%%| the Free Software Foundation, either version 3 of the License, or                      |
%%%| (at your option) any later version.                                                    |
%%%|                                                                                        |
%%%| This program is distributed in the hope that it will be useful,                        |
%%%| but WITHOUT ANY WARRANTY; without even the implied warranty of                         |
%%%| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                          |
%%%| GNU General Public License for more details.                                           |
%%%|                                                                                        |
%%%| You should have received a copy of the GNU General Public License                      |
%%%| along with this program.  If not, see <http://www.gnu.org/licenses/>.                  |
%%%|                                                                                        |
%%%| @author Gorka Suárez García                                                            |
%%%| @copyright (C) 2019-2021, Gorka Suárez García                                          |
%%%|========================================================================================|
-module(constraint).
-author("Gorka Suárez García").
-export([
    % Make Functions:
    match/2, subseteq/1, subseteq/2, joinable/1, bottom/0, conjunction/0,

    % Property Functions:
    get_type/1, get_left/1, get_right/1,

    % Query Functions:
    is_constraint/1, is_conjunction/1, is_emtpy/1,

    % Conjunctions Functions:
    insert/2, insert_match/3, insert_subseteq/3, insert_joinable/2, merge/2,
    unsafe_merge/2, partition/2, partition/3,

    % Definitions Functions:
    has_definition/2, get_definition/2, get_definition/3,
    get_definition_match/3, get_definition_subseteq/3,

    % Upgrade Functions:
    match_upgrade/1
]).
-include("type.hrl").

%%===========================================================================================
%% Make Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a match constraint.
%% @param Left The left type of the constraint.
%% @param Right The right type of the constraint.
%% @returns The new constraint definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
match(Left, Right) ->
    {?CONSTRAINT_MATCH, Left, Right}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a subset or equal constraint.
%% @param Left The left type of the constraint.
%% @param Right The right type of the constraint.
%% @returns The new constraint definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq(Left) ->
    {?CONSTRAINT_SUBSETEQ, Left, type:any()}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a subset or equal constraint.
%% @param Left The left type of the constraint.
%% @param Right The right type of the constraint.
%% @returns The new constraint definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq(Left, Right) ->
    {?CONSTRAINT_SUBSETEQ, Left, Right}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a joinable constraint.
%% @param Variables The variables of the constraint.
%% @returns The new constraint definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
joinable(Variables) when is_list(Variables) ->
    {?CONSTRAINT_JOINABLE, Variables}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the bottom value of constraints.
%% @returns The new bottom value of constraints.
%% @end
%%-------------------------------------------------------------------------------------------
bottom() -> ?CONSTRAINTS_BOTTOM.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of an empty conjunction of constraints.
%% @returns The new conjunction of constraints.
%% @end
%%-------------------------------------------------------------------------------------------
conjunction() -> [].

%%===========================================================================================
%% Property Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the class of a constraint.
%% @param Value The constraint value.
%% @returns The class of a constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_type({Class, _}) -> Class;
get_type({Class, _, _}) -> Class.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the left type of a constraint.
%% @param Value The constraint value.
%% @returns The left type of a constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_left({_, Left}) -> Left;
get_left({_, Left, _}) -> Left.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the right type of a constraint.
%% @param Value The constraint value.
%% @returns The right type of a constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_right({_, Right}) -> Right;
get_right({_, _, Right}) -> Right.

%%===========================================================================================
%% Query Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a constraint.
%% @param Value The value to check.
%% @returns 'true' if the value is a constraint; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_constraint({?CONSTRAINT_MATCH, Left, Right}) ->
    type:is_type(Left) andalso type:is_type(Right);
is_constraint({?CONSTRAINT_SUBSETEQ, Left, Right}) ->
    type:is_type(Left) andalso type:is_type(Right);
is_constraint({?CONSTRAINT_JOINABLE, Variables}) when is_list(Variables) ->
    lists:foldl(fun(Type, Accum) -> type:is_type(Type) and Accum end, true, Variables);
is_constraint(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a conjunction.
%% @param Value The value to check.
%% @returns 'true' if the value is a conjunction; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_conjunction([]) ->
    true;
is_conjunction([Value | Values]) ->
    case is_constraint(Value) of
        true -> is_conjunction(Values);
        false -> false
    end;
is_conjunction(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a conjunction is empty.
%% @param Value The value to check.
%% @returns 'true' if the value is an empty conjunction; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_emtpy(?CONSTRAINTS_BOTTOM) -> true;
is_emtpy([]) -> true;
is_emtpy(_) -> false.

%%===========================================================================================
%% Conjunctions Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Inserts a constraint value inside a container.
%% @param Container The container where add the value.
%% @param Value The value to insert inside the container.
%% @returns The modified container with the value inserted.
%% @end
%%-------------------------------------------------------------------------------------------
insert(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
insert(Container, []) ->
    Container;
insert(Container, [Value | Values]) ->
    insert(insert(Container, Value), Values);
insert(Container, Value) when is_tuple(Value) ->
    case is_constraint(Value) of
        false ->
            throw(invalid_value);
        true ->
            case is_conjunction(Container) of
                false -> throw(invalid_container);
                true -> lists:usort([Value | Container])
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Inserts a match constraint value inside a container.
%% @param Container The container where add the value.
%% @param Left The left value of the constraint to insert.
%% @param Right The right value of the constraint to insert.
%% @returns The modified container with the value inserted.
%% @end
%%-------------------------------------------------------------------------------------------
insert_match(Container, Left, Right) ->
    insert(Container, match(Left, Right)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Inserts a subset or equal constraint value inside a container.
%% @param Container The container where add the value.
%% @param Left The left value of the constraint to insert.
%% @param Right The right value of the constraint to insert.
%% @returns The modified container with the value inserted.
%% @end
%%-------------------------------------------------------------------------------------------
insert_subseteq(Container, Left, Right) ->
    insert(Container, subseteq(Left, Right)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Inserts a joinable constraint value inside a container.
%% @param Container The container where add the value.
%% @param Variables The variables of the constraint to insert.
%% @returns The modified container with the value inserted.
%% @end
%%-------------------------------------------------------------------------------------------
insert_joinable(Container, Variables) ->
    insert(Container, joinable(Variables)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a pair of conjunctions into one.
%% @param Left The left conjunction to merge.
%% @param Right The right conjunction to merge.
%% @returns The merged conjunction with both conjunctions.
%% @end
%%-------------------------------------------------------------------------------------------
merge(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
merge(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
merge(Left, Right) ->
    case {is_conjunction(Left), is_conjunction(Right)} of
        {true, true} ->
            case {Left, Right} of
                {[], _} ->
                    util:list_usort_flatten(Right);
                {_, []} ->
                    util:list_usort_flatten(Left);
                _ ->
                    util:list_usort_flatten(Left ++ Right)
            end;
        _ ->
            throw(invalid_container)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a pair of conjunctions into one.
%% @param Left The left conjunction to merge.
%% @param Right The right conjunction to merge.
%% @returns The merged conjunction with both conjunctions.
%% @end
%%-------------------------------------------------------------------------------------------
unsafe_merge(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
unsafe_merge(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
unsafe_merge(Left, Right) ->
    case {Left, Right} of
        {[], _} ->
            util:list_usort_flatten(Right);
        {_, []} ->
            util:list_usort_flatten(Left);
        _ ->
            util:list_usort_flatten(Left ++ Right)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes a partition in a set of constraints.
%% @param Constraints The constraints to split.
%% @param Predicate The predicate to use.
%% @returns A tuple with the constraints that makes true the predicate first
%%          and those that makes false the predicate second.
%% @end
%%-------------------------------------------------------------------------------------------
partition(?CONSTRAINTS_BOTTOM, _) ->
    {?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM};
partition(Constraints, Class) when is_list(Constraints), is_atom(Class) ->
    lists:partition(fun(C) -> get_type(C) =:= Class end, Constraints);
partition(Constraints, Predicate) when is_list(Constraints), is_function(Predicate) ->
    lists:partition(Predicate, Constraints);
partition(_, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes a partition in a set of constraints.
%% @param Constraints The constraints to split.
%% @param Class The type of the constraint to select.
%% @param RightType The constraint right type condition to select.
%% @returns A tuple with the constraints that makes true the predicate first
%%          and those that makes false the predicate second.
%% @end
%%-------------------------------------------------------------------------------------------
partition(?CONSTRAINTS_BOTTOM, _, _) ->
    {?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM};
partition(Constraints, all, RightType) ->
    lists:partition(
        fun(C) ->
            partition_type_filter(C, RightType)
        end,
        Constraints
    );
partition(Constraints, Class, RightType) ->
    lists:partition(
        fun(C) ->
            get_type(C) =:= Class andalso
                partition_type_filter(C, RightType)
        end,
        Constraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun partition/3'.
%% @end
%%-------------------------------------------------------------------------------------------
partition_type_filter(Constraint, Type) ->
    Victim = get_right(Constraint),
    case Type of
        ?TYPE_ANY -> type:is_any(Victim);
        ?TYPE_NONE -> type:is_none(Victim);
        ?TYPE_VARIABLE -> type:is_variable(Victim);
        ?TYPE_LITERAL -> type:is_literal(Victim);
        ?TYPE_SET -> type:is_set(Victim);
        ?TYPE_SCHEME -> type:is_scheme(Victim);
        ?TYPE_LAMBDA -> type:is_lambda(Victim);
        ?TYPE_CONDITION -> type:is_condition(Victim);
        ?TYPE_UNION -> type:is_union(Victim) orelse type:is_sequence(Victim);
        _ -> throw(not_supported)
    end.

%%===========================================================================================
%% Definitions Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type variable has a definition inside a set of constraints.
%% @param Constraints The Constraints to check.
%% @param Variable The name of the type variable to find.
%% @returns Returns 'true' when the type variable has a definition inside the constraints.
%% @end
%%-------------------------------------------------------------------------------------------
has_definition(Constraints, Variable) ->
    Name = type:get_variable_name(Variable),
    has_definition_inner(Constraints, Name).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun has_definition/2'.
%% @end
%%-------------------------------------------------------------------------------------------
has_definition_inner(?CONSTRAINTS_BOTTOM, _) ->
    false;
has_definition_inner([], _) ->
    false;
has_definition_inner([{?CONSTRAINT_MATCH, {?TYPE_VARIABLE, Name, _}, _} | _], Name) ->
    true;
has_definition_inner([{?CONSTRAINT_SUBSETEQ, {?TYPE_VARIABLE, Name, _}, _} | _], Name) ->
    true;
has_definition_inner([_ | Constraints], Name) ->
    has_definition_inner(Constraints, Name).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type of a definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Variable The name of the type variable to find.
%% @returns The found type, otherwise the type 'any()'.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition(Constraints, Variable) ->
    get_definition(Constraints, Variable, type:any()).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type of a definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Variable The name of the type variable to find.
%% @param Default The default value to return.
%% @returns The found type, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition(?CONSTRAINTS_BOTTOM, _, _) ->
    type:none();
get_definition(Constraints, Variable, Default) ->
    Name = type:get_variable_name(Variable),
    case find_definition_match(Constraints, Name, notfound) of
        notfound ->
            case find_definition_subseteq(Constraints, Name, notfound) of
                notfound ->
                    Default;
                SubseteqType ->
                    SubseteqType
            end;
        MatchType ->
            MatchType
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the type of a match definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Name The name of the type variable to find.
%% @param Default The default value to return.
%% @returns The found type, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition_match(Constraints, Variable, Default) ->
    Name = type:get_variable_name(Variable),
    find_definition_match(Constraints, Name, Default).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the type of a subseteq definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Name The name of the type variable to find.
%% @param Default The default value to return.
%% @returns The found type, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition_subseteq(Constraints, Variable, Default) ->
    Name = type:get_variable_name(Variable),
    find_definition_subseteq(Constraints, Name, Default).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the type of a match definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Name The name of the type variable to find.
%% @param Default The default value to return.
%% @returns The found type, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
find_definition_match(?CONSTRAINTS_BOTTOM, _, _) ->
    type:none();
find_definition_match([], _, Default) ->
    Default;
find_definition_match([Constraint | Constraints], Name, Default) ->
    case Constraint of
        {?CONSTRAINT_MATCH, {?TYPE_VARIABLE, Name, _}, Type} ->
            Type;
        _ ->
            find_definition_match(Constraints, Name, Default)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the type of a subseteq definition inside a set of constraints.
%% @param Constraints The set of constraints to inspect.
%% @param Name The name of the type variable to find.
%% @param Default The default value to return.
%% @returns The found type, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
find_definition_subseteq(?CONSTRAINTS_BOTTOM, _, _) ->
    type:none();
find_definition_subseteq([], _, Default) ->
    Default;
find_definition_subseteq([Constraint | Constraints], Name, Default) ->
    case Constraint of
        {?CONSTRAINT_SUBSETEQ, {?TYPE_VARIABLE, Name, _}, Type} ->
            Type;
        _ ->
            find_definition_subseteq(Constraints, Name, Default)
    end.

%%===========================================================================================
%% Upgrade Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply a match upgrade over subset constraints.
%% @param Constraints The constraints to change.
%% @returns The changed constraints.
%% @end
%%-------------------------------------------------------------------------------------------
match_upgrade(Constraints) ->
    util:list_map(
        Constraints,
        fun(Constraint) ->
            case Constraint of
                {?CONSTRAINT_SUBSETEQ, {?TYPE_VARIABLE, Name, Linked}, Right} ->
                    FinalClass =  case type:is_monomorphic(Right) of
                                      true -> ?CONSTRAINT_MATCH;
                                      _ -> ?CONSTRAINT_SUBSETEQ
                                  end,
                    {FinalClass, {?TYPE_VARIABLE, Name, Linked}, Right};
                Otherwise ->
                    Otherwise
            end
        end
    ).
