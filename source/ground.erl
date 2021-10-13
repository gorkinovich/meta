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
-module(ground).
-author("Gorka Suárez García").
-export([get_type/1, get_environment/1]).
-include("type.hrl").

%%===========================================================================================
%% Type Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the ground type for a polymorphic type.
%% @param Value The polymorphic type to transform.
%% @returns The final ground type.
%% @end
%%-------------------------------------------------------------------------------------------
get_type(Value) ->
    get_type(Value, environment:new()).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the ground type for a polymorphic type.
%% @param Value The polymorphic type to transform.
%% @param Table The environment with the ground types.
%% @returns The final ground type.
%% @end
%%-------------------------------------------------------------------------------------------
get_type(Value, Table) ->
    Victim = normalize:rename(Value),
    case get_environment(Victim, Table) of
        {_, ?CONSTRAINTS_BOTTOM, _} ->
            type:none();
        {NextValue, NextTable, _} ->
            normalize:recursive(change_type(NextValue, NextTable))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the ground type for a polymorphic type.
%% @param Value The polymorphic type to transform.
%% @param Table The environment with the ground types.
%% @returns The final ground type.
%% @end
%%-------------------------------------------------------------------------------------------
change_type(Value, Table) ->
    case Value of
        Value when is_list(Value) ->
            [change_type(Item, Table) || Item <- Value];
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(change_conditional_type(Type, Constraints, Table), []);
        {?TYPE_VARIABLE, Name, _} ->
            environment:get(Table, Name);
        {?TYPE_SET, Name, Parameters} ->
            type:set(Name, change_type(Parameters, Table));
        {?TYPE_SCHEME, _, Inner} ->
            change_type(Inner, Table);
        {?TYPE_LAMBDA, Parameters, Result} ->
            Tuple = type:tuple([Result | Parameters]),
            case get_type(Tuple, Table) of
                {?TYPE_SET, ?TYPE_TUPLE, [NR | NPS]} ->
                    type:lambda(NPS, NR);
                _ ->
                    type:lambda_none(length(Parameters))
            end;
        {?TYPE_CONDITION, Type, Constraints} ->
            change_conditional_type(Type, Constraints, Table);
        {?TYPE_UNION, Ordered, Types} ->
            type:union_or_sequence(Ordered, [get_type(Type, Table) || Type <- Types]);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            mono:subseteq(change_type(Variable, Table), change_type(Type, Table));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            mono:subseteq(change_type(LeftType, Table), change_type(RightType, Table));
        {?CONSTRAINT_JOINABLE, Variables} ->
            type:is_none(mono:infimum(change_type(Variables, Table)));
        {?TYPE_SYMBOL_CALL, _, _} ->
            throw(not_supported);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun change_type/2'.
%% @end
%%-------------------------------------------------------------------------------------------
change_conditional_type(Type, Constraints, Table) ->
    case util:flags_all(change_type(Constraints, Table)) of
        false -> type:none();
        _ -> change_type(Type, Table)
    end.

%%===========================================================================================
%% Environment Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the ground types environment for a polymorphic type.
%% @param Value The polymorphic type to query.
%% @returns The final ground types environment.
%% @end
%%-------------------------------------------------------------------------------------------
get_environment(Value) ->
    get_environment(normalize:rename(Value), environment:new()).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the ground types environment for a polymorphic type.
%% @param Value The polymorphic type to query.
%% @param Table The ground types environment.
%% @returns The final ground types environment.
%% @end
%%-------------------------------------------------------------------------------------------
get_environment(Value, Table) ->
    case get_top_constraints(Value) of
        {_, ?CONSTRAINTS_BOTTOM} ->
            {type:none(), ?CONSTRAINTS_BOTTOM, []};
        {Type, Constraints} ->
            {NTB, NTP, NCS, Aliases} = remove_aliases(Table, Type, Constraints),
            {NTP, get_environment(NTB, NCS, []), Aliases}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the ground types environment for a polymorphic type.
%% @param Table The ground types environment.
%% @param Constraints The constraints to transform.
%% @param Conditions The conditions to check.
%% @returns The final ground types environment.
%% @end
%%-------------------------------------------------------------------------------------------
get_environment(Table, [], Conditions) ->
    check_conditions(reduce_environment(Table), Conditions);
get_environment(Table, [Constraint | Constraints], Conditions) ->
    case Constraint of
        {?CONSTRAINT_MATCH, Variable, Type} ->
            get_environment(Table, Variable, Type, Constraints, Conditions);
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            case type:is_variable(LeftType) of
                true -> get_environment(Table, LeftType, RightType, Constraints, Conditions);
                _ -> get_environment(Table, Constraints, [Constraint | Conditions])
            end;
        _ ->
            get_environment(Table, Constraints, [Constraint | Conditions])
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun get_environment/3'.
%% @end
%%-------------------------------------------------------------------------------------------
get_environment(Table, LeftType, RightType, Constraints, Conditions) ->
    LeftName = type:get_variable_name(LeftType),
    NextTable = environment:append(Table, LeftName, RightType),
    get_environment(NextTable, Constraints, Conditions).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a types environment into a ground types environment.
%% @param Table The types environment to change.
%% @returns The final ground types environment.
%% @end
%%-------------------------------------------------------------------------------------------
reduce_environment(Table) ->
    Candidates = [{Name, query:get_variables(environment:get(Table, Name))}
        || Name <- maps:keys(Table)],
    reduce_environment(Table, table:sort_dependencies(Candidates)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun get_environment/3'.
%% @end
%%-------------------------------------------------------------------------------------------
reduce_environment(Table, []) ->
    Table;
reduce_environment(Table, [{_, Names} | Dependencies]) ->
    TempTable = clear_environment(Table, Names),
    NextTable = lists:foldl(
        fun(Name, Accum) ->
            Type = environment:get(Accum, Name),
            NextType = change_type(Type, TempTable),
            environment:set(Accum, Name, mono:infimum(NextType))
        end,
        Table,
        Names
    ),
    reduce_environment(NextTable, Dependencies).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun reduce_environment/2'.
%% @end
%%-------------------------------------------------------------------------------------------
clear_environment(Table, Names) ->
    lists:foldl(
        fun(Name, Accum) ->
            environment:set(Accum, Name, type:any())
        end,
        Table,
        Names
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a set of conditions are true to return an environment.
%% @param Table The ground types environment.
%% @param Conditions The conditions to check.
%% @returns The final ground types environment.
%% @end
%%-------------------------------------------------------------------------------------------
check_conditions(Table, Conditions) ->
    Victim = type:condition(type:any(), Conditions),
    Result = change_type(Victim, Table),
    case type:is_none(Result) of
        false -> Table;
        _ -> ?CONSTRAINTS_BOTTOM
    end.

%%===========================================================================================
%% Extract Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the constraints inside a type, with the exception of the constraints inside
%% the union and the lambda types, which will taken later in the ground transformation
%% function. Because lambda constraints only applies inside the lambda, and union
%% constraints vary between the types of the union.
%% @param Value The type to query.
%% @returns The constraints of the type.
%% @end
%%-------------------------------------------------------------------------------------------
get_top_constraints(Value) ->
    {NextValue, Constraints} = extract_constraints(Value),
    NextConstraints = util:list_usort_flatten(Constraints),
    case lists:member(?CONSTRAINTS_BOTTOM, NextConstraints) of
        true -> {type:none(), ?CONSTRAINTS_BOTTOM};
        _ -> {NextValue, NextConstraints}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun get_top_constraints/1'.
%% @end
%%-------------------------------------------------------------------------------------------
extract_constraints(Value) ->
    case Value of
        Value when is_list(Value) ->
            lists:unzip([extract_constraints(Item) || Item <- Value]);
        {?TYPE_PAIR, Type, Constraints} ->
            {RT, RTCS} = extract_constraints(Type),
            {RCS, RCSCS} = extract_constraints(Constraints),
            {type:pair(RT, []), [RTCS, RCS, RCSCS]};
        {?TYPE_SET, Name, Parameters} ->
            {RPS, RPSCS} = extract_constraints(Parameters),
            {type:set(Name, RPS), RPSCS};
        {?TYPE_SCHEME, _, Inner} ->
            extract_constraints(Inner);
        {?TYPE_CONDITION, Type, Constraints} ->
            {RT, RTCS} = extract_constraints(Type),
            {RCS, RCSCS} = extract_constraints(Constraints),
            {RT, [RTCS, RCS, RCSCS]};
        {?CONSTRAINT_MATCH, Variable, Type} ->
            {RT, RTCS} = extract_constraints(Type),
            {constraint:match(Variable, RT), RTCS};
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            {RRT, RRTCS} = extract_constraints(RightType),
            {constraint:subseteq(LeftType, RRT), RRTCS};
        {?TYPE_SYMBOL_CALL, _, _} ->
            throw(not_supported);
        Otherwise ->
            {Otherwise, []}
    end.

%%===========================================================================================
%% Aliases Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Removes the aliases constraints to unify the type variables.
%% @param Type The type to transform.
%% @param Constraints The constraints to transform.
%% @return The type and constraints transformed.
%% @end
%%-------------------------------------------------------------------------------------------
remove_aliases(Table, Type, Constraints) ->
    {ACS, NCS} = constraint:partition(Constraints, ?CONSTRAINT_MATCH, ?TYPE_VARIABLE),
    {SCS, OCS} = constraint:partition(NCS, ?CONSTRAINT_SUBSETEQ, ?TYPE_VARIABLE),
    {NACS, NSCS} = find_aliases(ACS, SCS),
    {FTB, FTP, FCS} = remove_aliases(NACS, Table, Type, OCS ++ NSCS),
    {FTB, FTP, FCS, NACS}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun remove_aliases/3'.
%% @end
%%-------------------------------------------------------------------------------------------
find_aliases(Aliases, Subseteqs) ->
    Table = table_add_dependencies(environment:new(), Aliases),
    NextTable = table_add_dependencies(Table, Subseteqs),
    Dependencies = table:sort_dependencies(maps:to_list(NextTable)),
    find_aliases(Dependencies, Aliases, Subseteqs).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun find_aliases/2'.
%% @end
%%-------------------------------------------------------------------------------------------
find_aliases([], Aliases, Subseteqs) ->
    {Aliases, Subseteqs};
find_aliases([Dependency | Dependencies], Aliases, Subseteqs) ->
    case Dependency of
        {recnode, Names} ->
            NSS = lists:filter(
                fun(Victim) ->
                    LeftName = type:get_variable_name(constraint:get_left(Victim)),
                    RightName = type:get_variable_name(constraint:get_right(Victim)),
                    (not lists:member(LeftName, Names)) orelse (not lists:member(RightName, Names))
                end,
                Subseteqs
            ),
            NAS = util:list_usort(Aliases ++ make_aliases(Names)),
            find_aliases(Dependencies, NAS, NSS);
        _ ->
            find_aliases(Dependencies, Aliases, Subseteqs)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun find_aliases/3'.
%% @end
%%-------------------------------------------------------------------------------------------
make_aliases([LeftName, RightName | Names]) ->
    [constraint:match(type:variable(LeftName), type:variable(RightName)) |
        make_aliases([RightName | Names])];
make_aliases(_) ->
    [].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun find_aliases/2'.
%% @end
%%-------------------------------------------------------------------------------------------
table_add_dependencies(Table, Constraints) ->
    lists:foldl(
        fun(Victim, Accum) ->
            LeftName = type:get_variable_name(constraint:get_left(Victim)),
            RightName = type:get_variable_name(constraint:get_right(Victim)),
            environment:append(Accum, LeftName, RightName)
        end,
        Table,
        Constraints
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun remove_aliases/3'.
%% @end
%%-------------------------------------------------------------------------------------------
remove_aliases([], Table, Type, Constraints) ->
    {Table, Type, Constraints};
remove_aliases([Alias | Aliases], Table, Type, Constraints) ->
    LeftName = type:get_variable_name(constraint:get_left(Alias)),
    RightName = type:get_variable_name(constraint:get_right(Alias)),
    RenameTuple = {LeftName, RightName},
    NAS = type:rename_free_variables(Aliases, RenameTuple),
    NTP = type:rename_free_variables(Type, RenameTuple),
    NCS = type:rename_free_variables(Constraints, RenameTuple),
    NTB = environment:rename(Table, RenameTuple),
    remove_aliases(NAS, NTB, NTP, NCS).
