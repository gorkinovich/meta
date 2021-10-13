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
-module(normalize).
-author("Gorka Suárez García").
-export([
    % General Functions:
    node/1, recursive/1, recursive/2, list/1, linked/1, rename/1,

    % Recursive Functions:
    noise_recursive/1, flatten_recursive/1, bottom_recursive/1,
    conditions_recursive/1, schemes_recursive/1, unions_recursive/1,
    unions_variables_recursive/1, unions_variables_remove_recursive/1,

    % Node Functions:
    noise/1, flatten/1, bottom/1, bottom_pairs/1, condition/1, scheme/1,
    union/1, union_variables/1, union_variables_remove/1
]).
-include("type.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the node of a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
node(Value) ->
    V2 = flatten(Value),
    V3 = bottom(V2),
    V4 = condition(V3),
    V5 = union(V4),
    scheme(V5).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
recursive(Value) ->
    list(type:map(list(Value), fun node/1)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes a type.
%% @param Value The value to change.
%% @param OnVisit The on visit handler.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
recursive(Value, OnVisit) ->
    list(type:map(list(Value), OnVisit)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes a list.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
list([]) ->
    [];
list(Values) when is_list(Values) ->
    case constraint:is_conjunction(Values) orelse type:is_antype(Values) of
        true -> util:list_usort_flatten(Values);
        false -> util:list_flatten(Values)
    end;
list(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the linked flags inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
linked(Value) ->
    lists:foldl(
        fun(Name, Victim) ->
            type:set_linked_flag(Victim, Name, true)
        end,
        Value,
        query:get_linked_variables(Value)
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the names of closed variables to avoid collisions.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
rename(Value) ->
    rename(Value, query:get_free_variables(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the names of closed variables to avoid collisions.
%% @param Value The value to change.
%% @param Names The names of the free variables.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
rename(Value, Names) ->
    case Value of
        Values when is_list(Values) ->
            [rename(Item, Names) || Item <- Values];
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(rename(Type, Names), rename(Constraints, Names));
        {?TYPE_SET, Name, Parameters} ->
            type:set(Name, rename(Parameters, Names));
        {?TYPE_SCHEME, Variables, Inner} ->
            CommonNames = util:list_intersection(Names, Variables),
            Table = data:get_rename_table(CommonNames),
            NextVariables = [util:map_get(Table, Variable, Variable) || Variable <- Variables],
            NextInner = type:rename_free_variables(Inner, Table),
            NextNames = util:list_usort_flatten(Names ++ NextVariables),
            type:scheme(NextVariables, rename(NextInner, NextNames));
        {?TYPE_LAMBDA, Parameters, Result} ->
            type:lambda(rename(Parameters, Names), rename(Result, Names));
        {?TYPE_CONDITION, Type, Constraints} ->
            type:condition(rename(Type, Names), rename(Constraints, Names));
        {?TYPE_UNION, Ordered, Types} ->
            type:union_or_sequence(Ordered, rename(Types, Names));
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint:match(rename(Variable, Names), rename(Type, Names));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            constraint:subseteq(rename(LeftType, Names), rename(RightType, Names));
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            type:symbol_call(rename(Variable, Names), rename(Parameters, Names));
        _ ->
            Value
    end.

%%===========================================================================================
%% Recursive Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the constraints to remove constraints with an 'any()' on the right side.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
noise_recursive(Value) ->
    type:map(Value, fun noise/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the lists inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
flatten_recursive(Value) ->
    type:map(Value, fun flatten/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the bottom types inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
bottom_recursive(Value) ->
    type:map(Value, fun bottom/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the conditional types inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
conditions_recursive(Value) ->
    type:map(Value, fun condition/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the schemes inside a type by clearing the unused variables inside them.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
schemes_recursive(Value) ->
    type:map(Value, fun scheme/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the conditional types inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
unions_recursive(Value) ->
    type:map(Value, fun union/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the conditional types inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
unions_variables_recursive(Value) ->
    type:map(Value, fun union_variables/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the conditional types inside a type.
%% @param Value The value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
unions_variables_remove_recursive(Value) ->
    type:map(Value, fun union_variables_remove/1).

%%===========================================================================================
%% Node Functions (Noise Remove)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the constraints to remove constraints with an 'any()' on the right side.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
noise(Victim) ->
    case Victim of
        {?TYPE_PAIR, Type, Constraints} ->
            {_, NCS} = constraint:partition(Constraints, all, ?TYPE_ANY),
            type:pair(Type, constraint:match_upgrade(NCS));
        {?TYPE_CONDITION, Type, Constraints} ->
            {_, NCS} = constraint:partition(Constraints, all, ?TYPE_ANY),
            type:condition(Type, constraint:match_upgrade(NCS));
        Otherwise ->
            Otherwise
    end.

%%===========================================================================================
%% Node Functions (Flatten)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the lists on the node of a type.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
flatten(Victim) ->
    case Victim of
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(Type, util:list_usort_flatten(Constraints));
        {?TYPE_SET, Name, RPS} ->
            type:set(Name, util:list_flatten(RPS));
        {?TYPE_SCHEME, RVS, RI} ->
            type:scheme(util:list_usort_flatten(RVS), RI);
        {?TYPE_LAMBDA, RPS, RR} ->
            type:lambda(util:list_flatten(RPS), RR);
        {?TYPE_CONDITION, RT, RCS} ->
            type:condition(RT, util:list_usort_flatten(RCS));
        {?TYPE_UNION, true, RTS} ->
            type:sequence(util:list_flatten(RTS));
        {?TYPE_UNION, false, RTS} ->
            type:union(util:list_usort_flatten(RTS));
        {?CONSTRAINT_JOINABLE, RVS} ->
            constraint:joinable(util:list_usort_flatten(RVS));
        {?TYPE_SYMBOL_CALL, RV, RPS} ->
            type:symbol_call(RV, util:list_flatten(RPS));
        Otherwise ->
            Otherwise
    end.

%%===========================================================================================
%% Node Functions (Bottom)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the bottom on the node of a type.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
bottom(Victim) ->
    case Victim of
        {?TYPE_PAIR, Type, Constraints} ->
            case type:is_none(Type) orelse Constraints =:= ?CONSTRAINTS_BOTTOM of
                true -> type:pair(type:none(), ?CONSTRAINTS_BOTTOM);
                false -> type:pair(Type, Constraints)
            end;
        {?TYPE_SET, Name, Parameters} ->
            case type:contains_none(Parameters) of
                true -> type:none();
                false -> type:set(Name, Parameters)
            end;
        {?TYPE_SCHEME, Variables, Inner} ->
            case type:is_none(Inner) of
                true -> type:none();
                false -> type:scheme(Variables, Inner)
            end;
        {?TYPE_LAMBDA, Parameters, Result} ->
            case type:contains_none(Parameters) orelse type:is_none(Result) of
                true -> type:lambda_none(length(Parameters));
                false -> type:lambda(Parameters, Result)
            end;
        {?TYPE_CONDITION, Type, Constraints} ->
            case type:is_none(Type) orelse Constraints =:= ?CONSTRAINTS_BOTTOM of
                true -> type:none();
                false -> type:condition(Type, Constraints)
            end;
        {?TYPE_UNION, Ordered, Types} ->
            bottom_union(Ordered, Types);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun bottom/1'.
%% @end
%%-------------------------------------------------------------------------------------------
bottom_union(Ordered, Types) when is_list(Types) ->
    case lists:filter(fun is_type_something/1, Types) of
        [] ->
            case lists:filter(fun is_not_none/1, Types) of
                [] -> type:none();
                FinalTypes -> type:union_or_sequence(Ordered, FinalTypes)
            end;
        [FinalType] ->
            FinalType;
        FinalTypes ->
            type:union_or_sequence(Ordered, FinalTypes)
    end;
bottom_union(_, Types) ->
    Types.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun bottom/1'.
%% @end
%%-------------------------------------------------------------------------------------------
is_type_something(Type) ->
    not (type:is_none(Type) orelse type:is_lambda_none(Type)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun bottom/1'.
%% @end
%%-------------------------------------------------------------------------------------------
is_not_none(Type) ->
    not type:is_none(Type).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the bottom on a list of types.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
bottom_pairs(Value) ->
    case type:is_antype(Value) of
        true when is_list(Value) ->
            case lists:filter(fun({_, T, _}) -> is_not_none(T) end, Value) of
                [] -> Value;
                NextValue -> NextValue
            end;
        _ ->
            Value
    end.

%%===========================================================================================
%% Node Functions (Conditions)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the conditional types on the node of a type.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
condition(Victim) ->
    case Victim of
        {?TYPE_PAIR, Type, Constraints} ->
            case condition_condtype(Type, Constraints) of
                {?TYPE_CONDITION, NTP, NCS} ->
                    type:pair(NTP, NCS);
                NextType ->
                    type:pair(NextType)
            end;
        {?TYPE_SET, ?TYPE_TUPLE, Parameters} ->
            case condition_params(Parameters, [], []) of
                {Parameters, []} ->
                    type:tuple(Parameters);
                {ITS, ICS} ->
                    type:condition(type:tuple(ITS), ICS)
            end;
        {?TYPE_SET, ?TYPE_NELIST, [Body, Tail]} ->
            {NBD, ECS} = condition_body(Body),
            case Tail of
                {?TYPE_CONDITION, TTP, TCS} ->
                    type:condition(type:nelist(NBD, TTP), constraint:merge(ECS, TCS));
                _ ->
                    type:condition(type:nelist(NBD, Tail), ECS)
            end;
        {?TYPE_LAMBDA, Parameters, Result} ->
            case condition_params(Parameters, [], []) of
                {Parameters, []} ->
                    type:lambda(Parameters, Result);
                {ITS, ICS} ->
                    type:lambda(ITS, condition_condtype(Result, ICS))
            end;
        {?TYPE_CONDITION, Type, Constraints} ->
            condition_condtype(Type, Constraints);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks for conditional types in a parameters list for 'fun conditions/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_body(Value) ->
    case Value of
        {?TYPE_CONDITION, ValueType, ValueConstraints} ->
            {ICS, ECS} = extract_constraints(ValueType, ValueConstraints),
            {type:condition(ValueType, ICS), ECS};
        _ ->
            {Value, []}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks for conditional types in a parameters list for 'fun conditions/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_params([], Types, Constraints) ->
    {lists:reverse(Types), Constraints};
condition_params([Value | Values], Types, Constraints) ->
    case Value of
        {?TYPE_LAMBDA, PS, {?TYPE_CONDITION, RIT, RICS}} ->
            {ICS, ECS} = extract_constraints(type:lambda(PS, RIT), RICS),
            NextValue = type:lambda(PS, type:condition(RIT, ICS)),
            condition_params(Values, [NextValue | Types], constraint:merge(ECS, Constraints));
        {?TYPE_CONDITION, IT, ICS} ->
            condition_params(Values, [IT | Types], constraint:merge(ICS, Constraints));
        Otherwise ->
            condition_params(Values, [Otherwise | Types], Constraints)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_condtype(Type, Constraints) ->
    case Type of
        {?TYPE_CONDITION, IT, ICS} ->
            FinalConstraints = constraint:merge(ICS, Constraints),
            type:condition(IT, condition_constraints(FinalConstraints));
        _ ->
            type:condition(Type, condition_constraints(Constraints))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_constraints(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
condition_constraints(Constraints) ->
    condition_constraints(Constraints, []).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_constraints(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
condition_constraints([], Result) ->
    lists:usort(Result);
condition_constraints([Constraint | Constraints], Result) ->
    {NCN, ECS} = case Constraint of
                     {?CONSTRAINT_MATCH, Left, Right} ->
                         condition_constraints(?CONSTRAINT_MATCH, Left, Right);
                     {?CONSTRAINT_SUBSETEQ, Left, Right} ->
                         condition_constraints(?CONSTRAINT_SUBSETEQ, Left, Right);
                     Otherwise ->
                         {Otherwise, []}
                 end,
    NextConstraints = constraint:merge(Constraints, ECS),
    condition_constraints(NextConstraints, [NCN | Result]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions_constraints/1'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_constraints(Class, Left, Right) ->
    case data:get_condition_relax() of
        true ->
            % WARNING: The soundness of the type system doesn't support this operation.
            %          It may provoke to add errors or just simply lose precision.
            condition_constraints_relax(Class, Left, Right);
        _ ->
            condition_constraints_strong(Class, Left, Right)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions_constraints/3'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_constraints_relax(Class, Left, Right) ->
    case Right of
        {?TYPE_CONDITION, InnerType, InnerConstraints} ->
            {{Class, Left, InnerType}, InnerConstraints};
        {?TYPE_LAMBDA, Parameters, {?TYPE_CONDITION, InnerType, InnerConstraints}} ->
            {NICS, NECS} = extract_constraints(type:lambda(Parameters, InnerType), InnerConstraints),
            NextValue = type:lambda(Parameters, type:condition(InnerType, NICS)),
            {{Class, Left, NextValue}, NECS};
        _ ->
            {{Class, Left, Right}, []}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Extracts constraints related to a given type under certain conditions.
%% @end
%%-------------------------------------------------------------------------------------------
extract_constraints(Type, Constraints) ->
    FVS = query:get_free_variables(Type),
    lists:partition(
        fun(Victim) ->
            case Victim of
                {Class, Left, Right} when Class =:= ?CONSTRAINT_MATCH; Class =:= ?CONSTRAINT_SUBSETEQ ->
                    LFVS = query:get_free_variables(Left),
                    RFVS = query:get_free_variables(Right),
                    LIVS = util:list_intersection(LFVS, FVS),
                    RIVS = util:list_intersection(RFVS, FVS),
                    LIVS =/= [] andalso RIVS =/= [];
                _ ->
                    true
            end
        end,
        Constraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun conditions_constraints/3'.
%% @end
%%-------------------------------------------------------------------------------------------
condition_constraints_strong(Class, Left, Right) ->
    case Right of
        {?TYPE_CONDITION, InnerType, InnerConstraints} ->
            {NICS, NECS} = extract_constraints_strong(InnerType, InnerConstraints, false),
            {{Class, Left, type:condition(InnerType, NICS)}, NECS};
        {?TYPE_LAMBDA, Parameters, {?TYPE_CONDITION, InnerType, InnerConstraints}} ->
            {NICS, NECS} = extract_constraints_strong(type:lambda(Parameters, InnerType), InnerConstraints, true),
            NextValue = type:lambda(Parameters, type:condition(InnerType, NICS)),
            {{Class, Left, NextValue}, NECS};
        _ ->
            {{Class, Left, Right}, []}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Extracts constraints related to a given type under certain conditions.
%% @end
%%-------------------------------------------------------------------------------------------
extract_constraints_strong(Type, Constraints, MatchFlag) ->
    CFVS = [N || {N, C} <- maps:to_list(query:count_free_variables(Constraints)), C > 1],
    TFVS = util:list_usort_append(query:get_free_variables(Type), CFVS),
    lists:partition(
        fun(Victim) ->
            case Victim of
                {?CONSTRAINT_MATCH, Left, Right} ->
                    LFVS = query:get_free_variables(Left),
                    RFVS = query:get_free_variables(Right),
                    LIVS = util:list_intersection(LFVS, TFVS),
                    RIVS = util:list_intersection(RFVS, TFVS),
                    MatchFlag andalso LIVS =/= [] andalso RIVS =/= [];
                {?CONSTRAINT_SUBSETEQ, Left, _} ->
                    LFVS = query:get_free_variables(Left),
                    LIVS = util:list_intersection(LFVS, TFVS),
                    LIVS =/= [];
                _ ->
                    true
            end
        end,
        Constraints
    ).

%%===========================================================================================
%% Node Functions (Scheme)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the type variables inside a scheme.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
scheme(Victim) ->
    case Victim of
        {?TYPE_SCHEME, Variables, Inner} ->
            type:clear_variables(type:scheme(Variables, Inner));
        Otherwise ->
            Otherwise
    end.

%%===========================================================================================
%% Node Functions (Unions)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the union types.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
union(Victim) ->
    case Victim of
        {?TYPE_UNION, false, Types} ->
            type:union(Types);
        {?TYPE_UNION, true, Types} ->
            type:sequence(Types);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Normalizes the variables constraints inside union types.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
union_variables(Victim) ->
    case Victim of
        {?TYPE_UNION, false, Types} ->
            type:union(union_variables(Types, []));
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun union_variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
union_variables([], _) ->
    [];
union_variables([Type | Types], PreviousTypes) ->
    ITV0 = query:get_free_variables(Type),
    ITV1 = query:get_instantiable_variables(Types),
    ITV2 = query:get_instantiable_variables(PreviousTypes),
    Variables = (ITV1 ++ ITV2) -- ITV0,
    FinalType = union_generate_none_constraints(Type, Variables),
    [FinalType | union_variables(Types, [Type | PreviousTypes])].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun union_variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
union_generate_none_constraints(Type, Variables) ->
    CS = [constraint:match(type:variable(Name), type:none()) || Name <- Variables],
    case Type of
        {?TYPE_CONDITION, IT, ICS} ->
            type:condition(IT, constraint:merge(ICS, CS));
        _ ->
            type:condition(Type, CS)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes the normalization of the variables constraints inside union types.
%% @param Value The value to normalize.
%% @returns The normalize value.
%% @end
%%-------------------------------------------------------------------------------------------
union_variables_remove(Victim) ->
    case Victim of
        {?TYPE_UNION, false, Types} ->
            NextTypes = [union_remove_none_constraints(Type) || Type <- Types],
            type:union(NextTypes);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun union_variables_remove/1'.
%% @end
%%-------------------------------------------------------------------------------------------
union_remove_none_constraints(Type) ->
    case Type of
        {?TYPE_CONDITION, IT, ICS} ->
            {NCS, OCS} = constraint:partition(ICS, ?CONSTRAINT_MATCH, ?TYPE_NONE),
            type:condition(IT, union_remove_none_constraints(NCS, OCS));
        _ ->
            Type
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun union_remove_none_constraints/1'.
%% @end
%%-------------------------------------------------------------------------------------------
union_remove_none_constraints([], Constraints) ->
    lists:usort(Constraints);
union_remove_none_constraints([Victim = {_, Variable, _} | Victims], Constraints) ->
    Name = type:get_variable_name(Variable),
    FVS = query:get_free_variables(Constraints),
    case lists:member(Name, FVS) of
        true -> union_remove_none_constraints(Victims, [Victim | Constraints]);
        _ -> union_remove_none_constraints(Victims, Constraints)
    end;
union_remove_none_constraints([_ | Victims], Constraints) ->
    union_remove_none_constraints(Victims, Constraints).
