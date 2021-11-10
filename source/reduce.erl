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
-module(reduce).
-author("Gorka Suárez García").
-export([execute/1]).
-include("type.hrl").
-include("language.hrl").
-include("reduce.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Changes a type to reduce the constraints inside.
%% @param Type The type to change.
%% @returns The type obtained from the tranformation.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Value) ->
    execute_and_normalize(Value, #status{}).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a type to reduce the constraints inside.
%% @param Type The type to change.
%% @param Status The status information of the process.
%% @returns The type obtained from the tranformation.
%% @end
%%-------------------------------------------------------------------------------------------
execute_and_normalize(Value, Status) ->
    V2 = normalize:recursive(Value),
    V3 = execute(V2, Status),
    V4 = normalize:recursive(V3),
    normalize:bottom_pairs(V4).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a type to reduce the constraints inside.
%% @param Type The type to change.
%% @param Status The status information of the process.
%% @returns The type obtained from the tranformation.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Value, Status) ->
    case Value of
        Values when is_list(Values) ->
            execute_list(Values, [], Status, fun execute/2);

        {?TYPE_PAIR, Type, Constraints} ->
            execute_conditional(Type, Constraints, Status, fun type:pair/2);

        {?TYPE_CONDITION, Type, Constraints} ->
            execute_conditional(Type, Constraints, Status, fun type:condition/2);

        {?TYPE_SET, Name, Parameters} ->
            NextParameters = execute(Parameters, Status),
            util:list_map(
                util:list_product(NextParameters),
                fun(FinalParameters) ->
                    type:set(Name, FinalParameters)
                end
            );

        {?TYPE_SCHEME, Variables, Inner} ->
            FVS = query:get_free_variables(Inner),
            MakeScheme =
                fun(Victim) ->
                    NFVS = query:get_free_variables(Victim),
                    NextVariables = Variables ++ (NFVS -- FVS),
                    type:scheme(NextVariables, Victim)
                end,
            type:sequence(util:list_flatten_map(
                execute(Inner, Status),
                fun(NextInner) ->
                    case NextInner of
                        {?TYPE_UNION, true, Types} ->
                            [MakeScheme(Type) || Type <- Types];
                        _ ->
                            MakeScheme(NextInner)
                    end
                end
            ));

        {?TYPE_LAMBDA, Parameters, Result} ->
            Substate = tools:update_status_freevars(Status, Parameters),
            type:sequence(util:list_flatten_map(
                execute(Result, Substate),
                fun(NextResult) ->
                    make_lambda(Parameters, NextResult, Status)
                end
            ));

        {?TYPE_UNION, Ordered, Types} ->
            NextTypes = execute(Types, Status),
            type:union_or_sequence(Ordered, util:list_flatten(NextTypes));

        {?CONSTRAINT_MATCH, Variable, Type} ->
            Substate = tools:update_status_freevars(Status, Variable),
            util:list_flatten_map(
                execute(Type, Substate),
                fun(NextType) ->
                    constraint:match(Variable, NextType)
                end
            );

        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            Substate = tools:update_status_freevars(Status, LeftType),
            util:list_flatten_map(
                execute(RightType, Substate),
                fun(NextRightType) ->
                    constraint:subseteq(LeftType, NextRightType)
                end
            );

        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun execute/2'.
%% @end
%%-------------------------------------------------------------------------------------------
execute_list([], _, _, _) ->
    [];
execute_list([Value | Values], AccumValues, Status, OnValue) ->
    NextValue = OnValue(Value, tools:update_status_freevars(Status, Values ++ AccumValues)),
    [NextValue | execute_list(Values, [NextValue | AccumValues], Status, OnValue)].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun execute/2'.
%% @end
%%-------------------------------------------------------------------------------------------
execute_conditional(Type, Constraints, Status, OnMake) ->
    util:list_flatten_map(
        change_constraints(Constraints, tools:update_status_freevars(Status, Type)),
        fun({NextConstraints, NextStatus}) ->
            Params = query:get_call_params_variables(Constraints),
            TempStatus = NextStatus#status{
                watchvars = util:list_usort_append(NextStatus#status.watchvars, Params),
                freevars = Status#status.freevars
            },
            FinalConstraints = util:list_flatten(NextConstraints),
            util:list_flatten_map(
                execute(Type, tools:update_status_freevars(TempStatus, FinalConstraints)),
                fun(FinalType) ->
                    case any_none_on_variables(Params, FinalType, FinalConstraints) of
                        true ->
                            make_conditional(type:none(), [], TempStatus, OnMake);
                        _ ->
                            make_conditional(FinalType, FinalConstraints, TempStatus, OnMake)
                    end
                end
            )
        end
    ).

%%===========================================================================================
%% None Check Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if any variable inside a list of names is a 'none()' type.
%% @end
%%-------------------------------------------------------------------------------------------
any_none_on_variables(Names, Type, Constraints) ->
    {_, Table, Aliases} = ground:get_environment(type:condition(Type, Constraints)),
    NextTable = environment:copy_with_aliases(Table, Aliases),
    util:flags_any([type:is_none(environment:get(NextTable, Name)) || Name <- Names]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets all the watch type variables inside a type or constraints.
%% @end
%%-------------------------------------------------------------------------------------------
get_all_watchvars(Victim, Status) ->
    get_all_watchvars(Victim, Status#status.watchvars, []).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets all the watch type variables inside a type or constraints.
%% @end
%%-------------------------------------------------------------------------------------------
get_all_watchvars(_, [], Result) ->
    util:list_usort_flatten(Result);
get_all_watchvars(Victim, [Name | WatchVars], Result) ->
    NextResult = util:list_usort([Name | Result]),
    NextNames = find_watchvars(Name, Victim),
    NextWatchVars = util:list_usort_append(WatchVars, NextNames) -- NextResult,
    get_all_watchvars(Victim, NextWatchVars, NextResult).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Finds all the watch type variables inside a type or constraints.
%% @end
%%-------------------------------------------------------------------------------------------
find_watchvars(Name, Type) ->
    util:list_usort_flatten(find_watchvars(Name, Type, false)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Finds all the watch type variables inside a type or constraints.
%% @end
%%-------------------------------------------------------------------------------------------
find_watchvars(Victim, Value, Flags) ->
    case Value of
        Value when is_list(Value) ->
            util:list_flatten([find_watchvars(Victim, Item, Flags) || Item <- Value]);
        {?TYPE_PAIR, Type, Constraints} ->
            NT = find_watchvars(Victim, Type, Flags),
            NCS = find_watchvars(Victim, Constraints, Flags),
            util:list_append(NT, NCS);
        {?TYPE_VARIABLE, Name, _Linked} ->
            case Flags of
                true -> [Name];
                _ -> []
            end;
        {?TYPE_LITERAL, _Literal} ->
            [];
        {?TYPE_SET, _Name, Parameters} ->
            find_watchvars(Victim, Parameters, Flags);
        {?TYPE_SCHEME, Variables, Inner} ->
            NI = find_watchvars(Victim, Inner, Flags),
            util:list_usort_flatten(NI) -- Variables;
        {?TYPE_LAMBDA, Parameters, Result} ->
            NPS = find_watchvars(Victim, Parameters, Flags),
            NR = find_watchvars(Victim, Result, Flags),
            util:list_append(NPS, NR);
        {?TYPE_CONDITION, Type, Constraints} ->
            NT = find_watchvars(Victim, Type, Flags),
            NCS = find_watchvars(Victim, Constraints, Flags),
            util:list_append(NT, NCS);
        {?TYPE_UNION, _Ordered, Types} ->
            find_watchvars(Victim, Types, Flags);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            case type:get_variable_name(Variable) of
                Victim -> find_watchvars(Victim, Type, true);
                _ -> find_watchvars(Victim, Type, Flags)
            end;
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            case type:get_variable_name(LeftType) of
                Victim -> find_watchvars(Victim, RightType, true);
                _ -> find_watchvars(Victim, RightType, Flags)
            end;
        {?CONSTRAINT_JOINABLE, Variables} ->
            find_watchvars(Victim, Variables, Flags);
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            NV = find_watchvars(Victim, Variable, Flags),
            NPS = find_watchvars(Victim, Parameters, Flags),
            util:list_append(NV, NPS);
        Otherwise ->
            Otherwise
    end.

%%===========================================================================================
%% Make Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a conditional type removing unique constraints.
%% @end
%%-------------------------------------------------------------------------------------------
make_conditional(_, ?CONSTRAINTS_BOTTOM, _, OnMake) ->
    OnMake(type:none(), ?CONSTRAINTS_BOTTOM);
make_conditional(Type, Constraints, Status, OnMake) ->
    {_, NextType, NextConstraints} = normalize:recursive(type:pair(Type, Constraints)),
    WatchVars = get_all_watchvars(type:pair(NextType, NextConstraints), Status),
    {FinalType, TempConstraints} = rule_substitution:execute(NextType, NextConstraints, Status),
    util:list_flatten_map(
        change_constraints(TempConstraints, Status),
        fun({FinalConstraints, _}) ->
            case any_none_on_variables(WatchVars, FinalType, FinalConstraints) of
                true -> OnMake(type:none(), []);
                _ -> apply_rule_kill_constraints(OnMake(FinalType, FinalConstraints), Status)
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a lambda type removing unique constraints.
%% @end
%%-------------------------------------------------------------------------------------------
make_lambda(Parameters, {?TYPE_CONDITION, _, ?CONSTRAINTS_BOTTOM}, _) ->
    type:lambda_none(length(Parameters));
make_lambda(Parameters, ResultType = {?TYPE_CONDITION, _, _}, Status) ->
    {?TYPE_CONDITION, Result, Constraints} = normalize:conditions_recursive(ResultType),
    Lambda = type:lambda(Parameters, Result),
    {NextType, NextConstraints} = rule_substitution:execute(Lambda, Constraints, Status#status{is_related = true}),
    {?TYPE_LAMBDA, NextParameters, NextResult} = NextType,
    type:lambda(NextParameters, type:condition(NextResult, NextConstraints));
make_lambda(Parameters, Result, _) ->
    type:lambda(Parameters, Result).

%%===========================================================================================
%% Constraints Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @return {Constraints, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_constraints(?CONSTRAINTS_BOTTOM, Status) ->
    {?CONSTRAINTS_BOTTOM, Status};
change_constraints(Constraints, Status) ->
    tools:check_and_make_table_constraints(
        Constraints,
        fun(Table) ->
            TDS = table:get_dependencies(Table),
            Order = table:sort_dependencies(TDS),
            change_constraints(Order, Table, Status)
        end,
        fun(_Table) ->
            {?CONSTRAINTS_BOTTOM, Status}
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @return {Constraints, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_constraints(_, ?CONSTRAINTS_BOTTOM, Status) ->
    {?CONSTRAINTS_BOTTOM, Status#status{ dirty = [] }};
change_constraints([], Table, Status) ->
    case Status#status.dirty of
        [] ->
            {table:get_constraints(Table), Status};
        Names ->
            change_constraints([{unknown, Names}], Table, Status#status{ dirty = [] })
    end;
change_constraints([Node | Nodes], Table, Status) ->
    util:list_map(
        change_constraints(Node, Table, Status),
        fun({NextTable, NextStatus}) ->
            change_constraints(Nodes, NextTable, NextStatus)
        end
    );
change_constraints({recnode, Names}, Table, Status) ->
    change_recursive_constraints(Names, Table, Status);
change_constraints({_, Names}, Table, Status) ->
    change_normal_constraints(Names, Table, Status);
change_constraints(_, _, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_recursive_constraints(Names, Table, Status) ->
    NextEnvironment = util:map_init_keys(Status#status.environment, Names, none),
    NextStatus = Status#status{ environment = NextEnvironment },
    util:list_flatten(change_step_recursive_constraints(data:get_rec_max_depth(), Names, Table, NextStatus)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_step_recursive_constraints(Count, Names, Table, Status) ->
    util:list_flatten_map(
        change_normal_constraints(Names, Table, Status),
        fun({NextTable, NextStatus}) ->
            {FinalTable, FinalStatus} = check_table_and_status(Names, NextTable, NextStatus, Status),
            case {Count, check_recursive_stop_condition(Names, Status, FinalStatus)} of
                {_, true} ->
                    change_step_recursive_constraints_debug("RECURSIVE STOP", Count, NextStatus, FinalStatus),
                    {FinalTable, FinalStatus};
                {0, false} ->
                    change_step_recursive_constraints_debug("RECURSIVE END", Count, NextStatus, FinalStatus),
                    {WTB, WST} = widening:execute(Names, FinalTable, FinalStatus),
                    change_step_recursive_constraints(0, Names, WTB, WST);
                {_, false} ->
                    change_step_recursive_constraints_debug("RECURSIVE STEP", Count, NextStatus, FinalStatus),
                    change_step_recursive_constraints(Count - 1, Names, Table, FinalStatus)
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun change_step_recursive_constraints/4'.
%% @end
%%-------------------------------------------------------------------------------------------
change_step_recursive_constraints_debug(Title, Count, NextStatus, FinalStatus) ->
    debug:environment(Title, [{"Count", Count, p1}], [{"Next", NextStatus#status.environment},
        {"Final", FinalStatus#status.environment}]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks and sets the current definitions with the previous ones.
%% @end
%%-------------------------------------------------------------------------------------------
check_table_and_status(Names, Table, Status, OldStatus) ->
    Environment = Status#status.environment,
    OldEnvironment = OldStatus#status.environment,
    Candidates = lists:map(
        fun(Name) ->
            Left = environment:get(Environment, Name),
            Right = environment:get(OldEnvironment, Name),
            case poly:subseteq(Left, Right) of
                true -> {Name, Right};
                _ -> {Name, Left}
            end
        end,
        Names
    ),
    {FinalTable, FinalEnvironment} = lists:foldl(
        fun({Name, Type}, {AccumTable, AccumEnvironment}) ->
            Constraints = lists:map(
                fun(Victim) ->
                    case Victim of
                        {Class, {?TYPE_VARIABLE, Name, Linked}, _} ->
                            {Class, {?TYPE_VARIABLE, Name, Linked}, Type};
                        Otherwise ->
                            Otherwise
                    end
                end,
                table:get_definition_constraints(AccumTable, Name)
            ),
            NextTable = table:set_definition_constraints(AccumTable, Name, Constraints),
            NextEnvironment = environment:set(AccumEnvironment, Name, Type),
            {NextTable, NextEnvironment}
        end,
        {Table, Environment},
        Candidates
    ),
    {FinalTable, Status#status{ environment = FinalEnvironment }}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @end
%%-------------------------------------------------------------------------------------------
check_recursive_stop_condition(Names, Status, NextStatus) ->
    Environment = Status#status.environment,
    NextEnvironment = NextStatus#status.environment,
    case util:map_equal_keys(Names, Environment, NextEnvironment) of
        true -> true;
        _ -> util:map_check_keys(Names, Environment, NextEnvironment, fun poly:equals/2)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_normal_constraints([], Table, Status) ->
    case are_conditions_bottom(Table) of
        true -> {?CONSTRAINTS_BOTTOM, Status};
        _ -> {table:remove_mono_conditions(Table), Status}
    end;
change_normal_constraints([Name | Names], Table, Status) ->
    util:list_flatten_map(
        change_definition_constraints(Name, Table, Status),
        fun({NextTable, NextStatus}) ->
            change_normal_constraints(Names, NextTable, NextStatus)
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
change_definition_constraints(Name, Table, Status) ->
    {_, DefCons, DefDeps} = table:get_definition(Table, Name),
    util:list_flatten_map(
        change_constraints_abstractions(DefCons, [], Status),
        fun({_, NextConstraints}) ->
            NextTable = table:set_definition(Table, {Name, NextConstraints, DefDeps}),
            util:list_flatten_map(
                apply_rules_constraints(Name, NextTable, Status),
                fun({FinalTable, FinalStatus}) ->
                    {FinalTable, tools:update_status(Name, FinalTable, FinalStatus)}
                end
            )
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes any inner abstraction in a set of constraints to reduce them.
%% @end
%%-------------------------------------------------------------------------------------------
change_constraints_abstractions(?CONSTRAINTS_BOTTOM, _, _) ->
    {'AceOfSpades', ?CONSTRAINTS_BOTTOM};
change_constraints_abstractions([], Result, _) ->
    {'KilledByDeath', lists:usort(Result)};
change_constraints_abstractions([Constraint | Constraints], Result, Status) ->
    util:list_flatten_map(
        case Constraint of
            {?CONSTRAINT_MATCH, Variable, RightType} ->
                execute_and_normalize(constraint:match(Variable, RightType), Status);
            {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
                execute_and_normalize(constraint:subseteq(LeftType, RightType), Status);
            Otherwise ->
                Otherwise
        end,
        fun(Value) ->
            change_constraints_abstractions(Constraints, [Value | Result], Status)
        end
    ).

%%===========================================================================================
%% Check Conditions Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if the conditions inside the constraints are bottom or not.
%% @end
%%-------------------------------------------------------------------------------------------
are_conditions_bottom(?CONSTRAINTS_BOTTOM) ->
    true;
are_conditions_bottom(Table) ->
    mono:is_bottom(get_mono_conditions(table:get_conditions(Table), Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms the conditions into a monomorphic condition constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_mono_conditions([], _) ->
    [];
get_mono_conditions([Condition | Conditions], Table) ->
    FinalCondition = get_mono_condition(Condition, Table),
    [FinalCondition | get_mono_conditions(Conditions, Table)].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms a condition into a monomorphic condition constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_mono_condition(Condition, Table) ->
    Names = query:get_variables(Condition),
    case Names of
        [] -> Condition;
        _ -> get_mono_condition(Names, Condition, Table)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Transforms a condition into a monomorphic condition constraint.
%% @end
%%-------------------------------------------------------------------------------------------
get_mono_condition([], Condition, Table) ->
    get_mono_condition(Condition, Table);
get_mono_condition([Name | Names], Condition, Table) ->
    NextType = table:get_definition_type(Table, Name),
    NextCondition = type:substitute_variable(Condition, Name, NextType),
    get_mono_condition(Names, NextCondition, Table).

%%===========================================================================================
%% Apply Rules Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to reduce them.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
apply_rules_constraints(Name, Table, Status) ->
    util:list_flatten_map(
        rule_symbol_call:execute(Name, Table, Status),
        fun({NextTable, NextStatus}) ->
            {NT2, NS2} = apply_rule_clear_constraints(Name, NextTable, NextStatus),
            apply_rule_infimum(Name, NT2, NS2)
        end
    ).

%%===========================================================================================
%% Clear Rule Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to clear constraints with an 'any()' type on the right side.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_clear_constraints(Name, Table, Status) ->
    Constraints = table:get_definition_constraints(Table, Name),
    NextConstraints = normalize:noise_recursive(Constraints),
    NextTable = table:set_definition_constraints(Table, Name, NextConstraints),
    {NextTable, Status}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to remove constraints unconnected.
%% @returns Type
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_kill_constraints(Type, Status) ->
    Names = [N || {N, C} <- maps:to_list(query:count_free_variables(Type)), C =< 1],
    Condition =
        fun(Victim) ->
            Name = type:get_variable_name(Victim),
            not lists:member(Name, Names) orelse lists:member(Name, Status#status.freevars)
        end,
    type:filter(
        Type,
        fun(Value) ->
            case Value of
                {?CONSTRAINT_MATCH, V, _} ->
                    Condition(V);
                {?CONSTRAINT_SUBSETEQ, L, _} ->
                    Condition(L);
                _ ->
                    true
            end
        end
    ).

%%===========================================================================================
%% Infimum Rule Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the infimum on constraints.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_infimum(Name, Table, Status) ->
    Constraints = table:get_definition_constraints(Table, Name),
    TempTable = table:set_definition_constraints(Table, Name, []),
    NextConstraints = constraint:match_upgrade(Constraints),
    {MCS, ZCS} = constraint:partition(NextConstraints, ?CONSTRAINT_MATCH),
    NMCS = apply_rule_infimum_sort_constraints(MCS),
    util:list_flatten_map(
        apply_rule_infimum(NMCS, ZCS),
        fun(InfimumResult) ->
            case InfimumResult of
                {constraints, FinalConstraints} ->
                    apply_rule_infimum_split_and_update(Name, Status, TempTable, FinalConstraints);
                FinalConstraints ->
                    apply_rule_infimum_split_and_update(Name, Status, TempTable, FinalConstraints)
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun apply_rule_infimum/3'.
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_infimum_sort_constraints(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
apply_rule_infimum_sort_constraints(Constraints) ->
    {TCS, FCS} = lists:partition(
        fun(Victim) ->
            case Victim of
                {?CONSTRAINT_MATCH, _, {?TYPE_LAMBDA, Parameters, Result}} ->
                    util:flags_all([type:is_variable(T) || T <- [Result | Parameters]]);
                _ ->
                    false
            end
        end,
        Constraints
    ),
    util:list_append(FCS, TCS).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun apply_rule_infimum/3'.
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_infimum_split_and_update(Name, Status, TempTable, FinalConstraints) ->
    {FinalTable, DirtyNames} = table:split_constraints(TempTable, FinalConstraints),
    FinalStatus = tools:update_status_dirty(Status, Name, DirtyNames),
    {FinalTable, FinalStatus}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to apply the infimum on constraints.
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_infimum(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
apply_rule_infimum(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
apply_rule_infimum([], Result) ->
    [{constraints, lists:usort(Result)}];
apply_rule_infimum([First | Constraints], Result) ->
    case Constraints of
        [] ->
            [{constraints, lists:usort([First | Result])}];
        [Second | Others] ->
            case {First, Second} of
                {{?CONSTRAINT_MATCH, Variable, Left}, {?CONSTRAINT_MATCH, Variable, Right}} ->
                    case type:is_variable(Left) andalso type:is_variable(Right) of
                        true ->
                            apply_rule_infimum([First | Others], [Second | Result]);
                        _ ->
                            case data:get_experimental() of
                                true ->
                                    apply_rule_infimum_call(Variable, Left, Right, Others, Result);
                                _ ->
                                    LVS = query:get_free_variables(Left),
                                    RVS = query:get_free_variables(Right),
                                    case util:list_intersection(LVS, RVS) of
                                        [] ->
                                            apply_rule_infimum_call(Variable, Left, Right, Others, Result);
                                        _ ->
                                            apply_rule_infimum([First | Others], [Second | Result])
                                    end

                            end
                    end;
                {{?CONSTRAINT_MATCH, _, _}, _} ->
                    apply_rule_infimum([First | Others], [Second | Result]);
                _ ->
                    apply_rule_infimum(Constraints, [First | Result])
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun apply_rule_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
apply_rule_infimum_call(Variable, Left, Right, Others, Result) ->
    util:list_flatten_map(
        poly:infimum(Left, Right),
        fun(Item) ->
            {FTP, FRS} = case Item of
                             {?TYPE_CONDITION, RT, RCS} ->
                                 {RT, constraint:merge(RCS, Result)};
                             RT ->
                                 {RT, Result}
                         end,
            apply_rule_infimum([constraint:match(Variable, FTP) | Others], FRS)
        end
    ).
