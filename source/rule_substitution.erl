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
-module(rule_substitution).
-author("Gorka Suárez García").
-export([recursive/1, execute/2, execute/3]).
-include("type.hrl").
-include("reduce.hrl").

%%===========================================================================================
%% Constant Values
%%===========================================================================================

-define(RULE_SUBSTITUTION_DEPTH, 3).

%%===========================================================================================
%% Recursive Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @param Value The value to change.
%% @returns The changed value.
%% @end
%%-------------------------------------------------------------------------------------------
recursive(Victim) ->
    type:map_wfvs(Victim,
        fun(Value, FreeVars) ->
            Status = #status{ freevars = FreeVars },
            case Value of
                {?TYPE_PAIR, Type, Constraints} ->
                    {NTP, NCS} = execute(Type, Constraints, Status),
                    type:pair(NTP, NCS);
                {?TYPE_CONDITION, Type, Constraints} ->
                    {NTP, NCS} = execute(Type, Constraints, Status),
                    type:condition(NTP, NCS);
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%===========================================================================================
%% Substitution Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @param Type The type to change.
%% @param Constraints The constraints to change.
%% @returns {Type, Constraints}
%% @end
%%-------------------------------------------------------------------------------------------
execute(Type, Constraints) ->
    execute(Type, Constraints, #status{}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @param Type The type to change.
%% @param Constraints The constraints to change.
%% @param Status The status of the reduce algorithm.
%% @returns {Type, Constraints}
%% @end
%%-------------------------------------------------------------------------------------------
execute(_, ?CONSTRAINTS_BOTTOM, _) ->
    {type:none(), ?CONSTRAINTS_BOTTOM};
execute(Type, Constraints, Status) ->
    execute_recursive(?RULE_SUBSTITUTION_DEPTH, Type, Constraints, Status).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
execute_recursive(0, Type, Constraints, _) ->
    {Type, Constraints};
execute_recursive(Count, Type, Constraints, Status) ->
    {NextType, NextConstraints} = execute_step(Type, Constraints, Status),
    case {NextType, NextConstraints} =:= {Type, Constraints} of
        true -> {NextType, NextConstraints};
        _ -> execute_recursive(Count - 1, NextType, NextConstraints, Status)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
execute_step(_, ?CONSTRAINTS_BOTTOM, _) ->
    {type:none(), ?CONSTRAINTS_BOTTOM};
execute_step(Type, Constraints, Status) ->
    data:set_is_related(Status#status.is_related),
    {NextType, NextConstraints} = execute(Type, [], Constraints, Status#status.freevars),
    remove_single_variables(NextType, NextConstraints, Status#status.freevars).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Type, Constraints, [], _) ->
    % After check all the constraints of the type, we have to check the monomorphic
    % conditions to obtain if the type is bottom or not, and return the final type:
    tools:check_and_make_table_constraints(
        Constraints,
        fun(Table) -> {Type, table:get_constraints(Table)} end,
        fun(_) -> {Type, ?CONSTRAINTS_BOTTOM} end
    );
execute(Type, Accum, [Current | Constraints], FreeVars) ->
    % If we have constraints to check, we'll check if the current constraint
    % is a match constraint to execute the substitution rule or not:
    case Current of
        {?CONSTRAINT_MATCH, Left, Right} ->
            % When we have a match constraint, we'll check if the right side is also
            % a type variable. When the right type isn't a type variable we try to
            % apply the substitution rule. Otherwise we have to sort the types
            % variables to select the one with lesser appearances in the type:
            case type:is_variable(Right) of
                false ->
                    execute(Left, Right, Type, Accum, Current, Constraints, FreeVars);
                _ ->
                    {LL, RR} = sort_variables(Left, Right, Type, Accum, Constraints),
                    execute(LL, RR, Type, Accum, Current, Constraints, FreeVars)
            end;
        _ ->
            % The other categories of constraints simply goes to the
            % accumulated final constraints of the final type:
            execute(Type, [Current | Accum], Constraints, FreeVars)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Left, Right, Type, Accum, Current, Constraints, FreeVars) ->
    % To apply the substitution rule, first we get the name of the type variable to remove,
    % and then we apply the substitution looking the name of the variable:
    Name = type:get_variable_name(Left),
    case execute(Name, Right, Type, Accum, Constraints, FreeVars) of
        % When we obtain the same input data, no substitution have been done. In this
        % case we get the current constraint and move it to the final constraints:
        {Type, Accum, Constraints} ->
            execute(Type, [Current | Accum], Constraints, FreeVars);

        % When we obtain changes from the imput data, we discard the curent constraint
        % and we continue to apply the next substitutions inside the type:
        {NextType, NextAccum, NextConstraints} ->
            execute(NextType, NextAccum, NextConstraints, FreeVars);

        % When there is no substitution, because the type variable only exists
        % in the current constraint, we discard the current constraint and move
        % into the next constraints to apply the next substitutions:
        count_zero ->
            execute(Type, Accum, Constraints, FreeVars);

        % Otherwise, we move the current constraint to the final constraints:
        _ ->
            execute(Type, [Current | Accum], Constraints, FreeVars)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
sort_variables(Left, Right, Type, Accum, Constraints) ->
    % We get the count of the types variables inside the type:
    LeftName = type:get_variable_name(Left),
    RightName = type:get_variable_name(Right),
    LeftCount = count_variable(LeftName, Type, Accum, Constraints),
    RightCount = count_variable(RightName, Type, Accum, Constraints),
    % If the right count is lesser than the left cout, we'll return the right
    % type first and the left second; otherwise we'll return the left type
    % first and the right second:
    case RightCount < LeftCount of
        true -> {Right, Left};
        _ -> {Left, Right}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Counts the number of appearances of a variable inside some types and constraints.
%% @end
%%-------------------------------------------------------------------------------------------
count_variable(Name, X, Y, Z) ->
    query:count_variable(X, Name)
        + query:count_variable(Y, Name)
        + query:count_variable(Z, Name).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the substitution over a type with constraints.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Name, Right, Type, LeftConstraints, RightConstraints, FreeVars) ->
    % We have to check if the current name to substitute isn't inside the
    % free variables outside the current type we're working on:
    case lists:member(Name, FreeVars) of
        true ->
            found_outside;
        _ ->
            % If the variable isn't free outside, we'll count the appearances inside
            % the type and the constraints we have, and check the sum of them:
            CTP = query:count_variable(Type, Name),
            CLC = query:count_variable(LeftConstraints, Name),
            CRC = query:count_variable(RightConstraints, Name),
            case (CTP + CLC + CRC) of
                % The variable only appears inside the current match constraint:
                0 ->
                    count_zero;
                % The variable appears one time in the type or the constraints, so
                % we'll in which part is the variable to execute the substitution:
                1 ->
                    case {CTP, CLC, CRC} of
                        {1, 0, 0} ->
                            {replace(Name, Right, Type, data:get_is_related()), LeftConstraints, RightConstraints};
                        {0, 1, 0} ->
                            {Type, replace(Name, Right, LeftConstraints, false), RightConstraints};
                        {0, 0, 1} ->
                            {Type, LeftConstraints, replace(Name, Right, RightConstraints, false)}
                    end;
                % The variable appears more than one time in the type, se we can't apply
                % the substitution, because we would lose precision in the final type:
                _ ->
                    count_overflow
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies a substitution over a type or constraints.
%% @end
%%-------------------------------------------------------------------------------------------
replace(Victim, Next, Value, Flag) ->
    % Fist we check which kind of type is the current value:
    case Value of
        % When we have a list of values, we apply the substitution inside each element:
        Values when is_list(Values) ->
            [replace(Victim, Next, Item, Flag) || Item <- Values];

        % When we have a pair type, we apply the substitution inside each field:
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(
                replace(Victim, Next, Type, Flag),
                replace(Victim, Next, Constraints, Flag)
            );

        % When we have a type variable, we check the name of the variable, and if
        % the name is the one we're looking for we return the substitution type:
        {?TYPE_VARIABLE, Name, Linked} ->
            case Name =:= Victim of
                true -> Next;
                _ -> type:variable(Name, Linked)
            end;

        % When we have a literal type, we return the same type:
        {?TYPE_LITERAL, Literal} ->
            type:literal(Literal);

        % When we have a set type, we check if the set is a nelist and then we apply
        % the substitution inside each parameter under certain conditions:
        {?TYPE_SET, Name, Parameters} ->
            case Name of
                ?TYPE_NELIST ->
                    [Body, Tail] = Parameters,
                    BodyFreeVars = query:get_free_variables(Body),
                    case lists:member(Victim, BodyFreeVars) of
                        true ->
                            NextFreeVars = query:get_free_variables(Next),
                            case util:list_intersection(BodyFreeVars, NextFreeVars) of
                                [] ->
                                    type:set(Name, replace(Victim, Next, Parameters, Flag));
                                _ ->
                                    case data:get_full_substitution() of
                                        true ->
                                            {Table, Constraints} = make_constraints(Body, Next),
                                            NextBody = type:rename(Body, Table),
                                            FinalBody = replace(Victim, Next, NextBody, Flag),
                                            FinalTail = replace(Victim, Next, Tail, Flag),
                                            type:condition(type:nelist(FinalBody, FinalTail), Constraints);
                                        _ ->
                                            type:set(Name, Parameters)
                                    end
                            end;
                        _ ->
                            type:set(Name, replace(Victim, Next, Parameters, Flag))
                    end;
                _ ->
                    type:set(Name, replace(Victim, Next, Parameters, Flag))
            end;

        % When we have a scheme type, we check the open variables, then we apply the substitution
        % inside the inner type, and finally we close the new variables in the final type:
        {?TYPE_SCHEME, Variables, Inner} ->
            % First, we rename all the closed variables that collides with the substitution:
            InputVars = [Victim | query:get_free_variables(Next)],
            Collision = util:list_intersection(InputVars, Variables),
            Table = data:get_rename_table(Collision),
            NextScheme = type:rename(type:scheme(Variables, Inner), Table),
            NextInner = type:get_inner_type(NextScheme),
            % Second, we get the free variables of the scheme and apply the substitution:
            OpenVars = query:get_free_variables(NextScheme),
            FinalInner = replace(Victim, Next, NextInner, Flag),
            % Then we get the free variables from the final inner type, remove the previous
            % free variables, and make a new scheme closing the new free variables:
            FreeVars = query:get_free_variables(FinalInner),
            FinalVars = FreeVars -- OpenVars,
            type:scheme(FinalVars, FinalInner);

        % When we have a lambda type, we apply the substitution inside each field:
        {?TYPE_LAMBDA, Parameters, Result} ->
            case Flag of
                % When the flag is true, we can execute directly the substitution because
                % the constraint is related to the result of the lambda type:
                true ->
                    type:lambda(
                        replace(Victim, Next, Parameters, false),
                        replace(Victim, Next, Result, false)
                    );
                % Otherwise, the constraint is inside a conditional type outside the
                % lambda, and we'll need to check if the free variables collides with
                % the free variables inside the substitution type we're working with:
                _ ->
                    NextFreeVars = query:get_free_variables(Next),
                    LambdaFreeVars = query:get_free_variables(type:lambda(Parameters, Result)),
                    case util:list_intersection(LambdaFreeVars, NextFreeVars) of
                        [] ->
                            type:lambda(
                                replace(Victim, Next, Parameters, Flag),
                                replace(Victim, Next, Result, Flag)
                            );
                        _ ->
                            % When we have a collision of variables, we can add new constraints to
                            % apply the substitution or we don't apply the substitution at all:
                            case data:get_full_substitution() of
                                true ->
                                    Lambda = type:lambda(Parameters, Result),
                                    {Table, Constraints} = make_constraints(Lambda, Next),
                                    {?TYPE_LAMBDA, NextParams, NextResult} = type:rename(Lambda, Table),
                                    FinalParams = replace(Victim, Next, NextParams, Flag),
                                    FinalResult = replace(Victim, Next, NextResult, Flag),
                                    type:condition(type:lambda(FinalParams, FinalResult), Constraints);
                                _ ->
                                    type:lambda(Parameters, Result)
                            end
                    end
            end;

        % When we have a conditional type, we apply the substitution inside each field:
        {?TYPE_CONDITION, Type, Constraints} ->
            type:condition(
                replace(Victim, Next, Type, Flag),
                replace(Victim, Next, Constraints, Flag)
            );

        % When we have a sequence type, we apply the substitution inside each type inside:
        {?TYPE_UNION, true, Types} ->
            type:sequence(replace(Victim, Next, Types, Flag));

        % When we have a union type, first we normalize the union variables, then we apply
        % the substitution inside each type inside, and then undo the normalization:
        {?TYPE_UNION, false, Types} ->
            {?TYPE_UNION, false, NormTypes} = normalize:union_variables({?TYPE_UNION, false, Types}),
            NextType = replace(Victim, Next, NormTypes, Flag),
            normalize:union_variables_remove(type:union(NextType));

        % When we have a subseteq constraint, we apply the substitution inside each field:
        {?CONSTRAINT_MATCH, Variable, Type} ->
            case type:is_variable(Next) of
                true ->
                    constraint:match(
                        replace(Victim, Next, Variable, Flag),
                        replace(Victim, Next, Type, Flag)
                    );
                _ ->
                    constraint:match(
                        Variable,
                        replace(Victim, Next, Type, Flag)
                    )
            end;

        % When we have a subseteq constraint, we apply the substitution inside each field:
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            case type:is_variable(Next) orelse type:is_literal(Next) of
                true ->
                    constraint:subseteq(
                        replace(Victim, Next, LeftType, Flag),
                        replace(Victim, Next, RightType, Flag)
                    );
                _ ->
                    constraint:subseteq(
                        LeftType,
                        replace(Victim, Next, RightType, Flag)
                    )
            end;

        % When we have a joinable constraint, we return the same constraint:
        {?CONSTRAINT_JOINABLE, Variables} ->
            constraint:joinable(Variables);

        % When we have a sybolic call type, we apply the substitution inside each field:
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            case type:is_variable(Next) of
                true ->
                    type:symbol_call(
                        replace(Victim, Next, Variable, Flag),
                        replace(Victim, Next, Parameters, Flag)
                    );
                _ ->
                    case type:is_literal(Next) of
                        true ->
                            type:symbol_call(
                                Variable,
                                replace(Victim, Next, Parameters, Flag)
                            );
                        _ ->
                            type:symbol_call(Variable, Parameters)
                    end
            end;

        % Otherwise we don't change the type:
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun substitution/4'.
%% @end
%%-------------------------------------------------------------------------------------------
make_constraints(Victim, Next) ->
    LKVS = lists:usort(query:get_linked_variables(Victim) ++ query:get_linked_variables(Next)),
    FVS1 = query:get_free_variables(Victim),
    FVS2 = query:get_free_variables(Next),
    Betas = util:list_intersection(FVS1, FVS2),
    Table = data:get_rename_table(Betas),
    Constraints = [tools:make_match_constraint(LKVS, maps:get(Beta, Table), Beta) || Beta <- Betas],
    {Table, Constraints}.

%%===========================================================================================
%% Remove Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Removes the singles variables in the type with the type 'any()'.
%% @end
%%-------------------------------------------------------------------------------------------
remove_single_variables(Type, Constraints, FreeVars) ->
    Pair = type:pair(Type, Constraints),
    NP1 = remove_single_variables(Pair, FreeVars),
    NP2 = normalize:noise_recursive(NP1),
    {_, NextType, NextConstraints} = normalize:recursive(NP2),
    {NextType, NextConstraints}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Removes the singles variables in the type with the type 'any()'.
%% @end
%%-------------------------------------------------------------------------------------------
remove_single_variables(Type, FreeVars) ->
    Table = query:count_free_variables(Type),
    Victims = get_single_variables_list(Table, FreeVars),
    type:map_wfvs(
        Type,
        fun(Value, FVS) ->
            case Value of
                {?TYPE_PAIR, Type, Constraints} ->
                    NextConstraints = filter_single_variables_constraints(Constraints, Victims),
                    type:pair(Type, NextConstraints);
                {?TYPE_VARIABLE, Name, _} ->
                    case lists:member(Name, Victims) of
                        true -> type:any();
                        _ -> Value
                    end;
                {?TYPE_SCHEME, Variables, Inner} ->
                    IFVS = FVS -- Variables,
                    NextInner = remove_single_variables(Inner, IFVS),
                    type:scheme(Variables, NextInner);
                {?TYPE_CONDITION, Type, Constraints} ->
                    NextConstraints = filter_single_variables_constraints(Constraints, Victims),
                    type:condition(Type, NextConstraints);
                Otherwise ->
                    Otherwise
            end
        end,
        FreeVars,
        false
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun remove_single_variables/2'.
%% @end
%%-------------------------------------------------------------------------------------------
get_single_variables_list(Table, FreeVars) ->
    lists:map(
        fun({Name, _}) -> Name end,
        lists:filter(
            fun(Victim) ->
                case Victim of
                    {Name, 1} -> not lists:member(Name, FreeVars);
                    _ -> false
                end
            end,
            maps:to_list(Table)
        )
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun remove_single_variables/2'.
%% @end
%%-------------------------------------------------------------------------------------------
filter_single_variables_constraints(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
filter_single_variables_constraints(Constraints, Names) ->
    Condition =
        fun(Victim) ->
            Name = type:get_variable_name(Victim),
            not (type:is_any(Victim) orelse lists:member(Name, Names))
        end,
    lists:filter(
        fun(Victim) ->
            case Victim of
                {?CONSTRAINT_MATCH, Variable, _} -> Condition(Variable);
                {?CONSTRAINT_SUBSETEQ, LeftType, _} -> Condition(LeftType);
                _ -> true
            end
        end,
        Constraints
    ).
