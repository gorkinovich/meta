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
-module(poly).
-author("Gorka Suárez García").
-export([equals/2, subset/2, subseteq/2, infimum/1, infimum/2, supremum/2]).
-include("type.hrl").
-include("query.hrl").

%%===========================================================================================
%% Check Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type is equal to another.
%% @param Left The left type to compare.
%% @param Right The right type to compare.
%% @returns 'true' if the types are equal; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
equals(Left, Right) ->
    subseteq(Left, Right) andalso subseteq(Right, Left).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type is subset to another.
%% @param Left The left type to compare.
%% @param Right The right type to compare.
%% @returns 'true' if the type is subset of the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subset(Left, Right) ->
    subseteq(Left, Right) andalso (not subseteq(Right, Left)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type is subset or equal to another.
%% @param Left The left type to compare.
%% @param Right The right type to compare.
%% @returns 'true' if the type is subset or equal to the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq(Left, Right) ->
    GroundLeft = ground:get_type(Left),
    GroundRight = ground:get_type(Right),
    case mono:subseteq(GroundLeft, GroundRight) of
        true -> subseteq_check_polymorphism(Left, Right);
        _ -> false
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a type is subset or equal to another.
%% @param Left The left type to compare.
%% @param Right The right type to compare.
%% @returns 'true' if the type is subset or equal to the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq_check_polymorphism(Left, Right) ->
    % Normalize both side to begin the process:
    NextLeft = normalize:rename(Left),
    NextRight = normalize:rename(Right),
    % Calculate the intersection of type variables and get a rename table:
    NLVS = query:get_variables(NextLeft),
    NRVS = query:get_variables(NextRight),
    Common = util:list_intersection(NLVS, NRVS),
    Table = data:get_rename_table(Common),
    % Get and solve the formulas that we'll use to check the connections:
    get_and_solve_formula(NextLeft, type:rename(NextRight, Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets and solves an input formula to check if it's true or not.
%% @end
%%-------------------------------------------------------------------------------------------
get_and_solve_formula({?TYPE_UNION, _, LTS}, Right) ->
    get_and_solve_formula_union_left(LTS, Right);
get_and_solve_formula(Left, {?TYPE_UNION, _, RTS}) ->
    get_and_solve_formula_union_right(Left, RTS);
get_and_solve_formula(Left, Right) ->
    Formula = collapse_and_get_formula(Left, Right),
    solve_formula(Formula).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets and solves an input formula to check if it's true or not.
%% @end
%%-------------------------------------------------------------------------------------------
get_and_solve_formula_union_left([], _) ->
    true;
get_and_solve_formula_union_left([Left | Values], Right) ->
    case get_and_solve_formula(Left, Right) of
        false -> false;
        _ -> get_and_solve_formula_union_left(Values, Right)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets and solves an input formula to check if it's true or not.
%% @end
%%-------------------------------------------------------------------------------------------
get_and_solve_formula_union_right(_, []) ->
    false;
get_and_solve_formula_union_right(Left, [Right | Values]) ->
    Formula = collapse_and_get_formula(Left, Right),
    case solve_formula(Formula) of
        true -> true;
        _ -> get_and_solve_formula_union_right(Left, Values)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Solves an input formula to check if it's true or not.
%% @end
%%-------------------------------------------------------------------------------------------
solve_formula({true, _, _}) ->
    true;
solve_formula({false, _, _}) ->
    false;
solve_formula({PS, LCS, RCS}) when is_list(PS) ->
    case lists:member(false, PS) of
        true ->
            false;
        _ ->
            case [Item || Item <- util:list_usort(PS), Item =/= true] of
                [] -> true;
                NPS -> check_conections(NPS, LCS, RCS)
            end
    end;
solve_formula(Formulas) when is_list(Formulas) ->
    solve_formula_list(Formulas);
solve_formula(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Solves an input formula to check if it's true or not.
%% @end
%%-------------------------------------------------------------------------------------------
solve_formula_list([]) ->
    false;
solve_formula_list([Formula | Formulas]) ->
    case solve_formula(Formula) of
        true -> true;
        _ -> solve_formula_list(Formulas)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks the connections for a predicate inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
check_conections(Predicates, LeftConstraints, RightConstraints) ->
    LCS = check_conections_filter_constraints(LeftConstraints),
    RCS = check_conections_filter_constraints(RightConstraints),
    check_conections_step(Predicates, LCS, RCS).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Filters the constraints for the check connections process.
%% @end
%%-------------------------------------------------------------------------------------------
check_conections_filter_constraints(Constraints) ->
    lists:filter(
        fun({_, Constraint}) ->
            Type = constraint:get_type(Constraint),
            Left = constraint:get_left(Constraint),
            Right = constraint:get_right(Constraint),
            (Type =:= ?CONSTRAINT_MATCH orelse Type =:= ?CONSTRAINT_SUBSETEQ)
                andalso not (type:is_monomorphic(Left) orelse type:is_monomorphic(Right))
        end,
        Constraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks the connections for a predicate inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
check_conections_step(Predicates, LeftConstraints, RightConstraints) ->
    {LeftList, RightList} = lists:unzip(Predicates),
    {NLL, NLCS} = transform_formulas(LeftList, LeftConstraints, maps:new(), []),
    {NRL, NRCS} = transform_formulas(RightList, RightConstraints, maps:new(), []),
    NPS = lists:zip(NLL, NRL),
    case NPS =/= Predicates of
        true ->
            check_conections_step(NPS, NLCS, NRCS);
        _ ->
            check_conections_expand(Predicates, LeftConstraints, RightConstraints)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Expands the connections for a predicate inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
check_conections_expand(Predicates, LeftConstraints, RightConstraints) ->
    Initial = [{true, LeftConstraints, RightConstraints}],
    Formulas = [collapse_and_get_formula(L, R) || {L, R} <- Predicates],
    NextFormulas = lists:foldl(fun merge_formula/2, Initial, Formulas),
    case check_stop_condition(NextFormulas, Predicates, LeftConstraints, RightConstraints) of
        true ->
            check_success_condition(Predicates);
        _ ->
            solve_formula(NextFormulas)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks the stop condition for the expansion of the connections.
%% @end
%%-------------------------------------------------------------------------------------------
check_stop_condition(NextFormulas, Predicates, LeftConstraints, RightConstraints) ->
    case NextFormulas of
        [{PD, LCS, RCS}] ->
            lists:usort(Predicates) =:= lists:usort(PD) andalso
                lists:usort(LeftConstraints) =:= lists:usort(LCS) andalso
                lists:usort(RightConstraints) =:= lists:usort(RCS);
        _ ->
            false
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks the success condition for the checking of the connections.
%% @end
%%-------------------------------------------------------------------------------------------
check_success_condition(Predicates) ->
    Table = lists:foldl(
        fun({Left, Right}, Accum) ->
            Values = maps:get(Right, Accum, []),
            FinalValues = util:list_usort_append(Values, Left),
            maps:put(Right, FinalValues, Accum)
        end,
        maps:new(),
        Predicates
    ),
    check_success_condition_list(maps:to_list(Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks the success condition for the checking of the connections.
%% @end
%%-------------------------------------------------------------------------------------------
check_success_condition_list([]) ->
    true;
check_success_condition_list([{RightType, LeftTypes} | Values]) ->
    case check_success_condition_reduce_list(LeftTypes) of
        [] ->
            check_success_condition_list(Values);
        [LeftType] ->
            {_, _, RightStatus} = RightType,
            {_, _, LeftStatus} = LeftType,
            case {LeftStatus, RightStatus} of
                {?VARIABLE_PLURAL, ?VARIABLE_SINGLE} ->
                    false;
                _ ->
                    check_success_condition_list(Values)
            end;
        _ ->
            false
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Reduces the left list of types when checking the success condition.
%% @end
%%-------------------------------------------------------------------------------------------
check_success_condition_reduce_list(Types) ->
    {Variables, Others} = lists:partition(
        fun(Item) -> type:is_variable(Item) end,
        Types
    ),
    NextOthers = check_success_condition_get_variables(Others),
    check_success_condition_reduce_list(Types, Variables, NextOthers).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Reduces the left list of types when checking the success condition.
%% @end
%%-------------------------------------------------------------------------------------------
check_success_condition_reduce_list(_, Variables, []) ->
    Variables;
check_success_condition_reduce_list(Types, Variables, NextOthers) when is_list(NextOthers) ->
    case Variables of
        [] ->
            [hd(NextOthers)];
        [Variable] ->
            case variables_list_intersection([Variable], NextOthers) of
                [TheChosenOne] -> [TheChosenOne];
                _ -> Types
            end;
        _ ->
            Types
    end;
check_success_condition_reduce_list(Types, _, _) ->
    Types.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Reduces the left list of types when checking the success condition.
%% @end
%%-------------------------------------------------------------------------------------------
check_success_condition_get_variables([]) ->
    [];
check_success_condition_get_variables(Types) ->
    {Unions, Others} = lists:partition(
        fun(Item) -> type:is_union(Item) end,
        Types
    ),
    case Others of
        [] ->
            Intersection = lists:foldl(
                fun({?TYPE_UNION, _, Victims}, Accum) ->
                    NextVictims = [V || V <- Victims, type:is_variable(V)],
                    case Accum of
                        nothing -> NextVictims;
                        _ -> variables_list_intersection(Accum, NextVictims)
                    end
                end,
                nothing,
                Unions
            ),
            case Intersection of
                nothing -> failure;
                [] -> failure;
                Values -> [variable_make_status(V) || V <- Values]
            end;
        _ ->
            failure
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the intersection between two lists of variables.
%% @end
%%-------------------------------------------------------------------------------------------
variables_list_intersection(Left, Right) ->
    LVS = query:get_variables(Left),
    RVS = query:get_variables(Right),
    Names = util:list_intersection(LVS, RVS),
    NLS = variables_list_filter(Left, Names),
    NRS = variables_list_filter(Right, Names),
    variables_list_reduce(util:list_usort_append(NLS, NRS)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Reduces a list of variables removing duplicates.
%% @end
%%-------------------------------------------------------------------------------------------
variables_list_reduce([]) ->
    [];
variables_list_reduce([Type | Types]) ->
    {NextType, NextTypes} = variables_list_reduce(Type, Types, []),
    [NextType | variables_list_reduce(NextTypes)].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun variables_list_reduce/1'.
%% @end
%%-------------------------------------------------------------------------------------------
variables_list_reduce(Victim, [], Types) ->
    {Victim, lists:reverse(Types)};
variables_list_reduce({?TYPE_VARIABLE, Name, LS}, [{?TYPE_VARIABLE, Name, RS} | Types], Accum) ->
    NLS = make_status(LS),
    NRS = make_status(RS),
    Status = query:variable_status_merge(NLS, NRS),
    variables_list_reduce({?TYPE_VARIABLE, Name, Status}, Types, Accum);
variables_list_reduce(Victim, [Type | Types], Accum) ->
    variables_list_reduce(Victim, Types, [Type | Accum]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Filters a list of variables with a list of names.
%% @end
%%-------------------------------------------------------------------------------------------
variables_list_filter(Victims, Names) ->
    lists:filter(
        fun({?TYPE_VARIABLE, Name, _}) ->
            lists:member(Name, Names)
        end,
        Victims
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms the connections for a predicate inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
transform_formulas([], Constraints, _, Result) ->
    {lists:reverse(Result), Constraints};
transform_formulas([Type | Types], Constraints, Table, Result) ->
    case type:is_variable(Type) of
        false ->
            transform_formulas(Types, Constraints, Table, [Type | Result]);
        _ ->
            case util:map_get(Table, Type, fatboy_slim) of
                fatboy_slim ->
                    CS = [C || {_, C} <- Constraints],
                    case constraint:get_definition_match(CS, Type, fatboy_slim) of
                        fatboy_slim ->
                            transform_formulas(Types, Constraints, Table, [Type | Result]);
                        CarlCox ->
                            Name = type:get_variable_name(Type),
                            NCS = transform_constraints(Type, CarlCox, Constraints),
                            NTB = maps:map(
                                fun(_, Victim) ->
                                    transform_type(Name, Victim, CarlCox)
                                end,
                                maps:put(Type, CarlCox, Table)
                            ),
                            NRS = lists:map(
                                fun(Victim) ->
                                    transform_type(Name, Victim, CarlCox)
                                end,
                                Result
                            ),
                            transform_formulas(Types, NCS, NTB, [CarlCox | NRS])
                    end;
                CarlCox ->
                    transform_formulas(Types, Constraints, Table, [CarlCox | Result])
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms some types for a set of constraints inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
transform_constraints(Type, Next, Constraints) ->
    TypeName = type:get_variable_name(Type),
    NextConstraints = lists:map(
        fun({Status, Victim}) ->
            case Victim of
                {?CONSTRAINT_MATCH, Left, Right} ->
                    NLT = transform_type(TypeName, Left, Next),
                    NRT = transform_type(TypeName, Right, Next),
                    {Status, {?CONSTRAINT_MATCH, NLT, NRT}};
                _ ->
                    {Status, Victim}
            end
        end,
        Constraints
    ),
    lists:filter(
        fun({_, Victim}) ->
            case Victim of
                {?CONSTRAINT_MATCH, {?TYPE_VARIABLE, Name, _}, {?TYPE_VARIABLE, Name, _}} ->
                    false;
                _ ->
                    true
            end
        end,
        NextConstraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms a type inside a formula.
%% @end
%%-------------------------------------------------------------------------------------------
transform_type(Name, Victim, Next) ->
    case Victim of
        {?TYPE_VARIABLE, Name, _} ->
            case type:is_variable(Next) of
                true -> Next;
                _ -> Victim
            end;
        _ ->
            Victim
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the formulas needed to check if some type is subset or equal than other.
%% @end
%%-------------------------------------------------------------------------------------------
collapse_and_get_formula(Left, Right) ->
    NextLeft = collapse_nelists_bodies(Left),
    NextRight = collapse_nelists_bodies(Right),
    get_formula(NextLeft, NextRight).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Collapses the bodies of the nelists inside a type.
%% @end
%%-------------------------------------------------------------------------------------------
collapse_nelists_bodies(Type) ->
    type:map(Type,
        fun(Value) ->
            case Value of
                {?TYPE_SET, ?TYPE_NELIST, [Left, {?TYPE_SET, ?TYPE_NELIST, [Right, Tail]}]} ->
                    {?TYPE_SET, ?TYPE_NELIST, [type:union(Left, Right), Tail]};
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the formulas needed to check if some type is subset or equal than other.
%% @end
%%-------------------------------------------------------------------------------------------
% Same types section:
get_formula(Type, Type) ->
    [{true, [], []}];
% 'none()' and 'any()' section:
get_formula(_, {?TYPE_SET, ?TYPE_ANY, _}) ->
    [{true, [], []}];
get_formula({?TYPE_SET, ?TYPE_NONE, _}, _) ->
    [{true, [], []}];
% 'tuple()' and 'fun()' section:
get_formula({?TYPE_SET, ?TYPE_TUPLE, _}, {?TYPE_SET, ?TYPE_TUPLE, []}) ->
    [{true, [], []}];
get_formula({?TYPE_LAMBDA, _, _}, {?TYPE_SET, ?TYPE_FUN, []}) ->
    [{true, [], []}];
% Tuple types section ({?TYPE_SET, ?TYPE_TUPLE, Parameters}):
get_formula({?TYPE_SET, ?TYPE_TUPLE, LPS}, {?TYPE_SET, ?TYPE_TUPLE, RPS}) when length(LPS) =:= length(RPS) ->
    lists:foldl(
        fun merge_formula/2,
        [{true, [], []}],
        [get_formula(LP, RP) || {LP, RP} <- lists:zip(LPS, RPS)]
    );
% Nelist types section ({?TYPE_SET, ?TYPE_NELIST, Parameters}):
get_formula({?TYPE_SET, ?TYPE_NELIST, [LB, LT]}, {?TYPE_SET, ?TYPE_NELIST, [RB, RT]}) ->
    F1 = get_formula(LB, RB),
    F2 = get_formula(LT, RT),
    F4 = get_formula(LT, {?TYPE_SET, ?TYPE_NELIST, [tools:make_shadow_type(RB), RT]}),
    util:list_append(
        merge_formula(F1, F2),
        merge_formula(F1, F4)
    );
% Lambda types section ({?TYPE_LAMBDA, Parameters, Result}):
get_formula({?TYPE_LAMBDA, LPS, LR}, {?TYPE_LAMBDA, RPS, RR}) when length(LPS) =:= length(RPS) ->
    lists:foldl(
        fun merge_formula/2,
        get_formula(LR, RR),
        [get_formula(LP, RP) || {LP, RP} <- lists:zip(LPS, RPS)]
    );
% Scheme types section ({?TYPE_SCHEME, Variables, Inner}):
get_formula({?TYPE_SCHEME, _, Left}, Right) ->
    get_formula(Left, Right);
get_formula(Left, {?TYPE_SCHEME, _, Right}) ->
    get_formula(Left, Right);
% Conditional types section ({?TYPE_CONDITION, Type, Constraints}):
get_formula({?TYPE_CONDITION, LT, LCS}, Right) ->
    NLCS = get_constraints({?TYPE_CONDITION, LT, LCS}),
    [{P, constraint:unsafe_merge(LC, NLCS), RC} || {P, LC, RC} <- get_formula(LT, Right)];
get_formula(Left, {?TYPE_CONDITION, RT, RCS}) ->
    NRCS = get_constraints({?TYPE_CONDITION, RT, RCS}),
    [{P, LC, constraint:unsafe_merge(RC, NRCS)} || {P, LC, RC} <- get_formula(Left, RT)];
% Type variables section ({?TYPE_VARIABLE, Name, Linked}):
get_formula(Left = {?TYPE_VARIABLE, _, _}, Right = {?TYPE_VARIABLE, _, _}) ->
    [make_formula(variable_make_status(Left), variable_make_status(Right))];
get_formula(Left = {?TYPE_VARIABLE, _, _}, Right) ->
    case Right of
        {?TYPE_UNION, _, RTS} ->
            util:list_flatten([get_formula(Left, RT) || RT <- RTS]);
        _ ->
            [make_formula(variable_make_status(Left), Right)]
    end;
get_formula(Left, Right = {?TYPE_VARIABLE, _, _}) ->
    [make_formula(Left, variable_make_status(Right))];
% Union types section ({?TYPE_UNION, Ordered, Types}):
get_formula({?TYPE_UNION, _, LTS}, Right) ->
    lists:foldl(
        fun merge_formula/2,
        [{true, [], []}],
        [get_formula(LT, Right) || LT <- LTS]
    );
get_formula(Left, {?TYPE_UNION, _, RTS}) ->
    util:list_flatten([get_formula(Left, RT) || RT <- RTS]);
% Default section:
get_formula(Left, Right) ->
    case type:is_monomorphic(Left) andalso type:is_monomorphic(Right) of
        true ->
            [{mono:subseteq(Left, Right), [], []}];
        _ ->
            [{false, ?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM}]
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a type varible to make a new status to be used inside.
%% @end
%%-------------------------------------------------------------------------------------------
variable_make_status({?TYPE_VARIABLE, Name, Linked}) ->
    {?TYPE_VARIABLE, Name, make_status(Linked)}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a new status to be used in the variables.
%% @end
%%-------------------------------------------------------------------------------------------
make_status(?VARIABLE_PLURAL) -> ?VARIABLE_PLURAL;
make_status(_) -> ?VARIABLE_SINGLE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a new simple formula.
%% @end
%%-------------------------------------------------------------------------------------------
make_formula(Left, Right) ->
    {LTP, LCS} = mask_mono_type(Left),
    {RTP, RCS} = mask_mono_type(Right),
    {[{LTP, RTP}], LCS, RCS}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Masks a monomorphic type when building a formula.
%% @end
%%-------------------------------------------------------------------------------------------
mask_mono_type(Type) ->
    case type:is_monomorphic(Type) of
        true ->
            Variable = {?TYPE_VARIABLE, data:get_identifier(), ?VARIABLE_SINGLE},
            {Variable, [{?VARIABLE_SINGLE, constraint:match(Variable, Type)}]};
        _ ->
            {Type, []}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the constraints needed to build a formula.
%% @end
%%-------------------------------------------------------------------------------------------
get_constraints({?TYPE_CONDITION, Type, Constraints}) ->
    Info = query:variables_simple({?TYPE_CONDITION, Type, Constraints}),
    lists:map(
        fun(Constraint) ->
            Status = lists:map(
                fun(Name) -> {_, S} = maps:get(Name, Info), S end,
                query:get_variables(Constraint)
            ),
            NextConstraint = type:map(
                Constraint,
                fun(Victim) ->
                    case Victim of
                        {?TYPE_VARIABLE, Name, _} ->
                            {_, S} = maps:get(Name, Info),
                            {?TYPE_VARIABLE, Name, S};
                        Otherwise ->
                            Otherwise
                    end
                end
            ),
            case lists:any(fun(Item) -> Item =:= ?VARIABLE_SINGLE end, Status) of
                true -> {?VARIABLE_SINGLE, NextConstraint};
                _ -> {?VARIABLE_PLURAL, NextConstraint}
            end
        end,
        Constraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Merges a couple of formulas.
%% @end
%%-------------------------------------------------------------------------------------------
merge_formula({LPS, LLCS, LRCS}, {RPS, RLCS, RRCS}) ->
    NPS = util:list_append(LPS, RPS),
    case lists:member(false, NPS) of
        true ->
            {false, ?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM};
        _ ->
            FLCS = constraint:unsafe_merge(LLCS, RLCS),
            FRCS = constraint:unsafe_merge(LRCS, RRCS),
            case [P || P <- NPS, P =/= true] of
                [] -> {true, FLCS, FRCS};
                FPS -> {FPS, FLCS, FRCS}
            end
    end;
merge_formula(Left, Right) ->
    case util:list_product(Left, Right) of
        [] ->
            [];
        Product ->
            Values = [merge_formula(L, R) || {L, R} <- Product],
            case [{PS, LCS, RCS} || {PS, LCS, RCS} <- Values, PS =/= false] of
                [] -> [{false, ?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM}];
                Otherwise -> Otherwise
            end
    end.

%%===========================================================================================
%% Operation Functions (Infimum)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the infimum between a list of types.
%% @param Values The types of the operation.
%% @returns The new type obtained from the operation.
%% @end
%%-------------------------------------------------------------------------------------------
infimum([]) ->
    type:none();
infimum([Value | Values]) ->
    lists:foldl(fun infimum/2, Value, Values);
infimum(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the infimum between two types.
%% @param Left The left type of the operation.
%% @param Right The right type of the operation.
%% @returns The new type and the new set of constraints obtained from the operation.
%% @end
%%-------------------------------------------------------------------------------------------
infimum(Left, Right) ->
    case check_infimum_exceptions(Left, Right) of
        {lambda, NL, NR} ->
            execute_lambda_infimum_exception(NL, NR);
        _ ->
            normalize:recursive(general_infimum(Left, Right))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
general_infimum(Left, Right) ->
    case type:is_monomorphic(Left) andalso type:is_monomorphic(Right) of
        true -> mono:infimum(Left, Right);
        false -> internal_infimum(Left, Right)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
% Same types section:
internal_infimum(Type, Type) ->
    Type;
% 'none()' and 'any()' section:
internal_infimum({?TYPE_SET, ?TYPE_NONE, _}, _) ->
    type:none();
internal_infimum(_, {?TYPE_SET, ?TYPE_NONE, _}) ->
    type:none();
internal_infimum({?TYPE_SET, ?TYPE_ANY, _}, Right) ->
    Right;
internal_infimum(Left, {?TYPE_SET, ?TYPE_ANY, _}) ->
    Left;
% Type variables section ({?TYPE_VARIABLE, Name, Linked}):
internal_infimum(Left = {?TYPE_VARIABLE, _, _}, Right = {?TYPE_VARIABLE, _, _}) ->
    type:condition(Left, [constraint:match(Left, Right)]);
internal_infimum(Left = {?TYPE_VARIABLE, _, _}, Right) ->
    type:condition(Left, [constraint:match(Left, Right)]);
internal_infimum(Left, Right = {?TYPE_VARIABLE, _, _}) ->
    type:condition(Right, [constraint:match(Right, Left)]);
% Conditional types section ({?TYPE_CONDITION, Type, Constraints}):
internal_infimum({?TYPE_CONDITION, Type, Constraints}, Right) ->
    type:condition(general_infimum(Type, Right), Constraints);
internal_infimum(Left, {?TYPE_CONDITION, Type, Constraints}) ->
    type:condition(general_infimum(Left, Type), Constraints);
% Tuple types section ({?TYPE_SET, ?TYPE_TUPLE, Parameters}):
internal_infimum({?TYPE_SET, ?TYPE_TUPLE, Left}, {?TYPE_SET, ?TYPE_TUPLE, Right}) ->
    case {Left, Right} of
        {[], _} -> type:tuple(Right);
        {_, []} -> type:tuple(Left);
        _ ->
            case length(Left) =/= length(Right) of
                true -> type:node();
                _ -> type:tuple([general_infimum(L, R) || {L, R} <- lists:zip(Left, Right)])
            end
    end;
% Nelist types section ({?TYPE_SET, ?TYPE_NELIST, Parameters}):
internal_infimum({?TYPE_SET, ?TYPE_NELIST, [LeftBody, LeftTail]}, {?TYPE_SET, ?TYPE_NELIST, [RightBody, RightTail]}) ->
    AVS = query:get_free_variables(LeftBody),
    BVS = query:get_free_variables(RightBody),
    RM1 = data:get_rename_table(AVS),
    RM2 = data:get_rename_table(AVS),
    RM3 = data:get_rename_table(BVS),
    RM4 = data:get_rename_table(BVS),
    LB1 = type:rename_free_variables(LeftBody, RM1),
    LB2 = type:rename_free_variables(LeftBody, RM2),
    RB1 = type:rename_free_variables(RightBody, RM3),
    RB2 = type:rename_free_variables(RightBody, RM4),
    type:union([
        type:nelist(
            general_infimum(LeftBody, RightBody),
            general_infimum(LeftTail, RightTail)
        ),
        type:condition(
            type:nelist(
                general_infimum(LB1, RightBody),
                general_infimum(type:nelist(LB2, LeftTail), RightTail)
            ),
            internal_list_constraints(AVS, RM1, RM2)
        ),
        type:condition(
            type:nelist(
                general_infimum(LeftBody, RB1),
                general_infimum(LeftTail, type:nelist(RB2, RightTail))
            ),
            internal_list_constraints(BVS, RM3, RM4)
        )
    ]);
% Lambda types section ({?TYPE_LAMBDA, Parameters, Result}):
internal_infimum(Left = {?TYPE_LAMBDA, _, _}, {?TYPE_SET, ?TYPE_FUN, []}) ->
    Left;
internal_infimum({?TYPE_SET, ?TYPE_FUN, []}, Right = {?TYPE_LAMBDA, _, _}) ->
    Right;
internal_infimum(Left = {?TYPE_LAMBDA, LPS, _}, Right = {?TYPE_LAMBDA, RPS, _}) when length(LPS) =:= length(RPS) ->
    LIVS = query:get_instantiable_variables(Left),
    RIVS = query:get_instantiable_variables(Right),
    LFVS = query:get_free_variables(Left),
    RFVS = query:get_free_variables(Right),
    AVS = util:list_intersection(LIVS, RIVS),
    BVS = util:list_intersection(LFVS, RFVS) -- AVS,
    RenMap = data:get_rename_table(util:list_append(AVS, BVS)),
    {?TYPE_LAMBDA, LeftParams, LeftResult} = type:rename_free_variables(Left, RenMap),
    {?TYPE_LAMBDA, RightParams, RightResult} = type:rename_free_variables(Right, RenMap),
    type:condition(
        type:lambda(
            [general_infimum(L, R) || {L, R} <- lists:zip(LeftParams, RightParams)],
            general_infimum(LeftResult, RightResult)
        ),
        constraint:merge(
            [make_match(Name, RenMap) || Name <- AVS],
            [[make_subseteq_left(Name, RenMap), make_subseteq_right(Name, RenMap)] || Name <- BVS]
        )
    );
% Scheme types section ({?TYPE_SCHEME, Variables, Inner}):
internal_infimum({?TYPE_SCHEME, LeftVariables, LeftInner}, {?TYPE_SCHEME, RightVariables, RightInner}) ->
    IVS = util:list_intersection(LeftVariables, RightVariables),
    case IVS of
        [] ->
            type:scheme(
                util:list_append(LeftVariables, RightVariables),
                general_infimum(LeftInner, RightInner)
            );
        _ ->
            RenMap = data:get_rename_table(IVS),
            NRVS = [util:map_get(RenMap, Item, Item) || Item <- RightVariables],
            NRT = type:rename_free_variables(RightInner, RenMap),
            type:scheme(
                util:list_append(LeftVariables, NRVS),
                general_infimum(LeftInner, NRT)
            )
    end;
internal_infimum({?TYPE_SCHEME, Variables, Inner}, Right) ->
    internal_scheme_and_type(Variables, Inner, Right);
internal_infimum(Left, {?TYPE_SCHEME, Variables, Inner}) ->
    internal_scheme_and_type(Variables, Inner, Left);
% Union types section ({?TYPE_UNION, Ordered, Types}):
internal_infimum({?TYPE_UNION, true, Types}, Right) ->
    type:sequence([general_infimum(Type, Right) || Type <- Types]);
internal_infimum(Left, {?TYPE_UNION, true, Types}) ->
    type:sequence([general_infimum(Left, Type) || Type <- Types]);
internal_infimum({?TYPE_UNION, false, Types}, Right) ->
    type:union([general_infimum(Type, Right) || Type <- Types]);
internal_infimum(Left, {?TYPE_UNION, false, Types}) ->
    type:union([general_infimum(Left, Type) || Type <- Types]);
% Default section:
internal_infimum(_, _) ->
    type:none().

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun internal_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
internal_scheme_and_type(Variables, InnerType, OtherType) ->
    IVS = util:list_intersection(Variables, query:get_free_variables(OtherType)),
    case IVS of
        [] ->
            type:scheme(Variables, general_infimum(InnerType, OtherType));
        _ ->
            RenMap = data:get_rename_table(IVS),
            NextType = type:rename_free_variables(OtherType, RenMap),
            type:scheme(Variables, general_infimum(InnerType, NextType))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun internal_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
internal_list_constraints(VS, T1, T2) ->
    util:list_usort_flatten([internal_list_constraints_step(Name, T1, T2) || Name <- VS]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun internal_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
internal_list_constraints_step(Name, T1, T2) ->
    [
        constraint:subseteq(
            type:variable(Name),
            type:union([
                make_variable(Name, T1),
                make_variable(Name, T2)
            ])
        ),
        make_subseteq_right(Name, T1),
        make_subseteq_right(Name, T2)
    ].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a variable with the data inside a renaming table.
%% @end
%%-------------------------------------------------------------------------------------------
make_variable(Name, Table) ->
    type:variable(util:map_get(Table, Name, Name)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a match constraint with the data inside a renaming table.
%% @end
%%-------------------------------------------------------------------------------------------
make_match(Name, Table) ->
    constraint:match(type:variable(Name), make_variable(Name, Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a subset or equal constraint with the data inside a renaming table.
%% @end
%%-------------------------------------------------------------------------------------------
make_subseteq_left(Name, Table) ->
    constraint:subseteq(type:variable(Name), make_variable(Name, Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a subset or equal constraint with the data inside a renaming table.
%% @end
%%-------------------------------------------------------------------------------------------
make_subseteq_right(Name, Table) ->
    constraint:subseteq(make_variable(Name, Table), type:variable(Name)).

%%===========================================================================================
%% Operation Functions (Exceptions)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if an infimum exception rule can be apply.
%% @end
%%-------------------------------------------------------------------------------------------
check_infimum_exceptions(Left, Right) ->
    case {check_lambda_type(Left), check_lambda_type(Right)} of
        {{lambda, variables}, {lambda, _}} ->
            {lambda, Left, Right};
        {{lambda, _}, {lambda, variables}} ->
            {lambda, Right, Left};
        _ ->
            none
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun check_infimum_exceptions/2'.
%% @end
%%-------------------------------------------------------------------------------------------
check_lambda_type(Victim) ->
    case type:is_abstraction(Victim) of
        true ->
            case Victim of
                {?TYPE_LAMBDA, Parameters, Result} ->
                    case util:flags_all([type:is_variable(T) || T <- [Result | Parameters]]) of
                        true -> {lambda, variables};
                        _ -> {lambda, simple}
                    end;
                {?TYPE_SCHEME, _, _} ->
                    {lambda, scheme};
                _ ->
                    {lambda, complex}
            end;
        _ ->
            none
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Executes the lambda infimum exception rule.
%% @end
%%-------------------------------------------------------------------------------------------
execute_lambda_infimum_exception(Left, Right) ->
    case check_lambda_type(Right) of
        {lambda, complex} ->
            execute_lambda_infimum_exception_union(Left, Right);
        {lambda, scheme} ->
            execute_lambda_infimum_exception_scheme(Left, Right);
        _ ->
            execute_lambda_infimum_exception_inner(Left, Right)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun execute_lambda_infimum_exception/2'.
%% @end
%%-------------------------------------------------------------------------------------------
execute_lambda_infimum_exception_inner(Left, Right) ->
    {?TYPE_LAMBDA, LPS, LR} = Left,
    {?TYPE_LAMBDA, RPS, RR} = Right,
    Victims = lists:zip([LR | LPS], [RR | RPS]),
    Constraints = lists:map(
        fun({Variable, Type}) ->
            constraint:match(Variable, Type)
        end,
        Victims
    ),
    type:condition(Right, Constraints).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun execute_lambda_infimum_exception/2'.
%% @end
%%-------------------------------------------------------------------------------------------
execute_lambda_infimum_exception_scheme(Left, Right) ->
    {?TYPE_SCHEME, Variables, Inner} = Right,
    RenMap = data:get_rename_table(Variables),
    NextInner = type:rename(Inner, RenMap),
    execute_lambda_infimum_exception_inner(Left, NextInner).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun execute_lambda_infimum_exception/2'.
%% @end
%%-------------------------------------------------------------------------------------------
execute_lambda_infimum_exception_union(Left, Right) ->
    {?TYPE_UNION, _Ordered, Types} = Right,
    lists:map(
        fun(Victim) ->
            case Victim of
                {?TYPE_CONDITION, _, Constraints} ->
                    {?TYPE_CONDITION, Right, Constraints};
                _ ->
                    throw(not_supported)
            end
        end,
        [execute_lambda_infimum_exception(Left, Type) || Type <- Types]
    ).

%%===========================================================================================
%% Operation Functions (Supremum)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the supremum between two types.
%% @param Left The left type of the operation.
%% @param Right The right type of the operation.
%% @returns The new type obtained from the operation.
%% @end
%%-------------------------------------------------------------------------------------------
supremum(Left, Right) ->
    normalize:unions_recursive(type:union(Left, Right)).
