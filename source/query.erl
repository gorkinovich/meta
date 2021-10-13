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
-module(query).
-author("Gorka Suárez García").
-export([
    % Names Functions:
    get_variables/1, get_free_variables/1,
    get_non_free_variables/1, get_instantiable_variables/1,
    get_linked_variables/1, get_unlinked_variables/1,
    get_call_params_variables/1, get_call_name_variables/1,

    % Count Functions:
    count_variable/2, count_variables/1, count_variables/2,
    count_free_variables/1, count_all_variables/1,
    count_all_free_variables/1, count_all_non_free_variables/1,

    % Variable Info Functions:
    variables_simple/1, free_variables_simple/1, non_free_variables_simple/1,
    variables/1, free_variables/1, non_free_variables/1,

    % Height Functions:
    get_height/1,

    % Utility Functions:
    varinfo_to_simple/1, variable_status_merge/2
]).
-include("type.hrl").
-include("query.hrl").

%%===========================================================================================
%% Names Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_variables(Value) ->
    util:list_usort_flatten(type:map(Value, fun get_variables_default/1)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun get_variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
get_variables_default(Victim) ->
    case Victim of
        {?TYPE_PAIR, RT, RCS} ->
            util:list_append(RT, RCS);
        {?TYPE_VARIABLE, Name, _} ->
            [Name];
        {?TYPE_LITERAL, _} ->
            [];
        {?TYPE_SET, _, RPS} ->
            RPS;
        {?TYPE_SCHEME, RVS, RI} ->
            util:list_append(RVS, RI);
        {?TYPE_LAMBDA, RPS, RR} ->
            util:list_append(RPS, RR);
        {?TYPE_CONDITION, RT, RCS} ->
            util:list_append(RT, RCS);
        {?TYPE_UNION, _, RTS} ->
            RTS;
        {?CONSTRAINT_MATCH, RL, RR} ->
            util:list_append(RL, RR);
        {?CONSTRAINT_SUBSETEQ, RL, RR} ->
            util:list_append(RL, RR);
        {?CONSTRAINT_JOINABLE, RVS} ->
            RVS;
        ?CONSTRAINTS_BOTTOM ->
            [];
        {?TYPE_SYMBOL_CALL, RV, RPS} ->
            util:list_append(RV, RPS);
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the free variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_free_variables(Value) ->
    util:list_usort_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_SCHEME, RVS, RI} ->
                    RI2 = util:list_usort_flatten(RI),
                    RVS2 = util:list_usort_flatten(RVS),
                    RI2 -- RVS2;
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the non free variable names inside a type.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_non_free_variables(Value) ->
    get_variables(Value) -- get_free_variables(Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the instantiable variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_instantiable_variables(Value) ->
    util:list_usort_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_SCHEME, RVS, RI} ->
                    RI2 = util:list_usort_flatten(RI),
                    RVS2 = util:list_usort_flatten(RVS),
                    RI2 -- RVS2;
                {?CONSTRAINT_MATCH, _, RR} ->
                    RR;
                {?CONSTRAINT_SUBSETEQ, _, _} ->
                    [];
                {?CONSTRAINT_JOINABLE, _} ->
                    [];
                {?TYPE_SYMBOL_CALL, _, _} ->
                    [];
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the linked variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_linked_variables(Value) ->
    util:list_usort_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, true} ->
                    [Name];
                {?TYPE_VARIABLE, _, false} ->
                    [];
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the unlinked variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_unlinked_variables(Value) ->
    util:list_usort_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, _, true} ->
                    [];
                {?TYPE_VARIABLE, Name, false} ->
                    [Name];
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variable names inside the parameters of a call symbol.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_call_params_variables(Value) ->
    util:list_usort_flatten(get_cpvs_map(Value, false)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The map function for 'fun get_call_params_variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
get_cpvs_map(Value, Flag) ->
    case Value of
        Values when is_list(Values) ->
            [get_cpvs_map(Item, Flag) || Item <- Values];
        {?TYPE_PAIR, Type, Constraints} ->
            util:list_append(get_cpvs_map(Type, Flag), get_cpvs_map(Constraints, Flag));
        {?TYPE_VARIABLE, Name, _} ->
            case Flag of
                true -> [Name];
                _ -> []
            end;
        {?TYPE_LITERAL, _} ->
            [];
        {?TYPE_SET, _, Parameters} ->
            get_cpvs_map(Parameters, Flag);
        {?TYPE_SCHEME, Variables, Inner} ->
            ITNS = util:list_usort_flatten(get_cpvs_map(Inner, Flag)),
            VSNS = util:list_usort_flatten(get_cpvs_map(Variables, true)),
            ITNS -- VSNS;
        {?TYPE_LAMBDA, Parameters, Result} ->
            util:list_append(get_cpvs_map(Parameters, Flag), get_cpvs_map(Result, Flag));
        {?TYPE_CONDITION, Type, Constraints} ->
            util:list_append(get_cpvs_map(Type, Flag), get_cpvs_map(Constraints, Flag));
        {?TYPE_UNION, _, Types} ->
            get_cpvs_map(Types, Flag);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            util:list_append(get_cpvs_map(Variable, Flag), get_cpvs_map(Type, Flag));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            util:list_append(get_cpvs_map(LeftType, Flag), get_cpvs_map(RightType, Flag));
        {?CONSTRAINT_JOINABLE, Variables} ->
            get_cpvs_map(Variables, Flag);
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            util:list_append(get_cpvs_map(Variable, Flag), get_cpvs_map(Parameters, true));
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variable names inside the names of a call symbol.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_call_name_variables(Value) ->
    util:list_usort_flatten(get_cnvs_map(Value, false)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The map function for 'fun get_call_name_variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
get_cnvs_map(Value, Flag) ->
    case Value of
        Values when is_list(Values) ->
            [get_cnvs_map(Item, Flag) || Item <- Values];
        {?TYPE_PAIR, Type, Constraints} ->
            util:list_append(get_cnvs_map(Type, Flag), get_cnvs_map(Constraints, Flag));
        {?TYPE_VARIABLE, Name, _} ->
            case {Flag, language:is_bif(Name)} of
                {true, false} -> [Name];
                _ -> []
            end;
        {?TYPE_LITERAL, _} ->
            [];
        {?TYPE_SET, _, Parameters} ->
            get_cnvs_map(Parameters, Flag);
        {?TYPE_SCHEME, Variables, Inner} ->
            ITNS = util:list_usort_flatten(get_cnvs_map(Inner, Flag)),
            VSNS = util:list_usort_flatten(get_cnvs_map(Variables, true)),
            ITNS -- VSNS;
        {?TYPE_LAMBDA, Parameters, Result} ->
            util:list_append(get_cnvs_map(Parameters, Flag), get_cnvs_map(Result, Flag));
        {?TYPE_CONDITION, Type, Constraints} ->
            util:list_append(get_cnvs_map(Type, Flag), get_cnvs_map(Constraints, Flag));
        {?TYPE_UNION, _, Types} ->
            get_cnvs_map(Types, Flag);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            util:list_append(get_cnvs_map(Variable, Flag), get_cnvs_map(Type, Flag));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            util:list_append(get_cnvs_map(LeftType, Flag), get_cnvs_map(RightType, Flag));
        {?CONSTRAINT_JOINABLE, Variables} ->
            get_cnvs_map(Variables, Flag);
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            util:list_append(get_cnvs_map(Variable, true), get_cnvs_map(Parameters, Flag));
        Otherwise ->
            Otherwise
    end.

%%===========================================================================================
%% Count Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of appearances of a variable inside a type or constraint.
%% @param Value The value to analyze.
%% @param Name The name of the variable.
%% @returns The number of appearances of a variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_variable(Value, Name) ->
    util:list_sum(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                Name -> 1;
                {?TYPE_PAIR, RT, RCS} -> RT + util:list_sum(RCS);
                {?TYPE_VARIABLE, Name, _} -> 1;
                {?TYPE_SET, _, RPS} -> util:list_sum(RPS);
                {?TYPE_SCHEME, RVS, RI} -> util:list_sum(RVS) + RI;
                {?TYPE_LAMBDA, RPS, RR} -> util:list_sum(RPS) + RR;
                {?TYPE_CONDITION, RT, RCS} -> RT + util:list_sum(RCS);
                {?TYPE_UNION, _, RTS} -> util:list_sum(RTS);
                {?CONSTRAINT_MATCH, RL, RR} -> RL + RR;
                {?CONSTRAINT_SUBSETEQ, RL, RR} -> RL + RR;
                {?CONSTRAINT_JOINABLE, RVS} -> util:list_sum(RVS);
                {?TYPE_SYMBOL_CALL, RV, RPS} -> RV + util:list_sum(RPS);
                _ -> 0
            end
        end
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of variables inside a type or constraint.
%% @param Value The value to analyze.
%% @returns The map with the count of each variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_variables(Value) ->
    count_variables(Value, false).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of variables inside a type or constraint.
%% @param Value The value to analyze.
%% @param SchemeFlag The count scheme variables flag.
%% @returns The map with the count of each variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_variables(Value, SchemeFlag) ->
    count_join_list(util:list_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, _} ->
                    [{Name, 1}];
                {?TYPE_SCHEME, RVS, RI} ->
                    case SchemeFlag of
                        true ->
                            NRVS = [{V, 1} || V <- RVS],
                            util:list_append(NRVS, RI);
                        _ ->
                            RI
                    end;
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    ))).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of free variables inside a type or constraint.
%% @param Value The value to analyze.
%% @returns The map with the count of each variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_free_variables(Value) ->
    count_join_list(util:list_flatten(type:map(
        Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, _} ->
                    [{Name, 1}];
                {?TYPE_SCHEME, RVS, RI} ->
                    Names = util:list_usort_flatten(RVS),
                    lists:filter(
                        fun({Name, _}) ->
                            not lists:member(Name, Names)
                        end,
                        util:list_flatten(RI)
                    );
                Otherwise ->
                    get_variables_default(Otherwise)
            end
        end
    ))).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Takes an input list of tuples to make a map with the count of each variable.
%% @param Values The list of values to join.
%% @returns The map with the count of each variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_join_list(Values) ->
    lists:foldl(
        fun({Name, Count}, Accum) ->
            util:map_update(Accum, Name, 0, fun(V) -> V + Count end)
        end,
        maps:new(),
        Values
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of variables inside a type or constraint.
%% @param Value The value to analyze.
%% @returns The number of variables inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_all_variables(Value) ->
    length(get_variables(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of free variables inside a type or constraint.
%% @param Value The value to analyze.
%% @returns The number of free variables inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_all_free_variables(Value) ->
    length(get_free_variables(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Counts the number of non free variables inside a type or constraint.
%% @param Value The value to analyze.
%% @returns The number of non free variables inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
count_all_non_free_variables(Value) ->
    length(get_non_free_variables(Value)).

%%===========================================================================================
%% Variable Info Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variables' information inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
variables_simple(Value) ->
    varinfo_to_simple(variables(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the free variables' information inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
free_variables_simple(Value) ->
    varinfo_to_simple(free_variables_simple(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the non free variables' information inside a type.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
non_free_variables_simple(Value) ->
    varinfo_to_simple(non_free_variables(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variables' information inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
variables(Value) ->
    general_join_list(util:list_flatten(variables_default(Value))).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun variables/1'.
%% @end
%%-------------------------------------------------------------------------------------------
variables_default(Value) ->
    case Value of
        Value when is_list(Value) ->
            [variables_default(Item) || Item <- Value];
        {?TYPE_PAIR, Type, Constraints} ->
            conditional_variables_default(Type, Constraints);
        {?TYPE_VARIABLE, Name, _Linked} ->
            [#varinfo{name = Name}];
        {?TYPE_SET, Name, Parameters} ->
            case Name of
                ?TYPE_NELIST ->
                    [RBD, RTL] = variables_default(Parameters),
                    [variable_status_relax(RBD), RTL];
                _ ->
                    variables_default(Parameters)
            end;
        {?TYPE_SCHEME, Variables, Inner} ->
            RVS = [#varinfo{name = Name, status = ?VARIABLE_PLURAL} || Name <- Variables],
            RI = variables_default(Inner),
            util:list_append(RVS, RI);
        {?TYPE_LAMBDA, Parameters, Result} ->
            RPS = variables_default(Parameters),
            RR = variables_default(Result),
            variable_status_relax(util:list_append(RPS, RR));
        {?TYPE_CONDITION, Type, Constraints} ->
            conditional_variables_default(Type, Constraints);
        {?TYPE_UNION, _Ordered, Types} ->
            variables_default(Types);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint_variables_default(?CONSTRAINT_MATCH, Variable, Type);
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            constraint_variables_default(?CONSTRAINT_SUBSETEQ, LeftType, RightType);
        {?CONSTRAINT_JOINABLE, Variables} ->
            RVS = variables_default(Variables),
            NRVS = variable_set_location(RVS, ?LOCATION_CONSTRAINT),
            variable_set_status(NRVS, ?VARIABLE_PLURAL);
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            RV = variables_default(Variable),
            RPS = variables_default(Parameters),
            util:list_append(RV, RPS);
        _ ->
            []
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun variables_default/1'.
%% @end
%%-------------------------------------------------------------------------------------------
conditional_variables_default(Type, Constraints) ->
    RT = variables_default(Type),
    RCS = variables_default(Constraints),
    util:list_append(RT, RCS).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun variables_default/1'.
%% @end
%%-------------------------------------------------------------------------------------------
constraint_variables_default(Class, LeftType, RightType) ->
    RL = variables_default(LeftType),
    RR = variables_default(RightType),
    case type:is_literal(RightType) of
        % The right side of the constraint is a literal, so the left side is singular,
        % that's why we'll set the location as a singleton case, because the instance
        % of the variable only can be one single value:
        true ->
            variable_set_location(RL, ?LOCATION_SINGLETON);

        % The right side of the constraint contains type variables:
        _ ->
            % First we get the names and their location in each part:
            LeftNames = variable_get_name(RL),
            RightNames = variable_get_name(RR),
            LeftLocation = constraint_get_location(Class, ?LOCATION_SUBSETEQ_LEFT),
            RightLocation = constraint_get_location(Class, ?LOCATION_SUBSETEQ_RIGHT),
            LeftRelatives = {LeftLocation, LeftNames},
            RightRelatives = {RightLocation, RightNames},
            % Second we update the records of each part with the new data:
            NRL = variable_add_relatives_and_location(RL, ?LOCATION_CONSTRAINT, RightRelatives),
            NRR = variable_add_relatives_and_location(RR, ?LOCATION_CONSTRAINT, LeftRelatives),
            % Finally, we'll join both sides and relax the status:
            variable_status_relax(util:list_append(NRL, NRR))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Takes an input list of tuples to make a map with the data of each variable.
%% @param Values The list of values to join.
%% @returns The map with the data of each variable inside the value.
%% @end
%%-------------------------------------------------------------------------------------------
general_join_list(Values) ->
    Table = lists:foldl(
        fun(Current, Accum) ->
            util:map_update(
                Accum, Current#varinfo.name,
                #varinfo{ name = Current#varinfo.name, count = 0, status = ?VARIABLE_PLURAL},
                fun(Item) -> variable_data_merge(Current, Item) end
            )
        end,
        maps:new(),
        Values
    ),
    general_update_status(Table, maps:keys(Table)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_join_list/1'.
%% @end
%%-------------------------------------------------------------------------------------------
general_update_status(Table, []) ->
    Table;
general_update_status(Table, [Name | Names]) ->
    {NextTable, Visited} = general_update_status(Table, Name, Names, []),
    general_update_status(NextTable, Names -- Visited).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_update_status/2'.
%% @end
%%-------------------------------------------------------------------------------------------
general_update_status(Table, _, [], Visited) ->
    {Table, Visited};
general_update_status(Table, Root, Candidates, Visited) ->
    RootInfo = maps:get(Root, Table),
    general_update_status(Table, RootInfo#varinfo.status, RootInfo#varinfo.relatives,
        Candidates, util:list_usort_append(Root, Visited)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_update_status/4'.
%% @end
%%-------------------------------------------------------------------------------------------
general_update_status(Table, Status, [], _, Visited) ->
    NextTable = lists:foldl(
        fun(Current, Accum) ->
            Info = maps:get(Current, Accum),
            NextStatus = variable_status_merge(Info#varinfo.status, Status),
            maps:put(Current, Info#varinfo{ status = NextStatus}, Accum)
        end,
        Table,
        Visited
    ),
    {NextTable, Visited};
general_update_status(Table, Status, Relatives, Candidates, Visited) ->
    {Matches, Others} = lists:partition(fun({Class, _}) -> Class =:= ?LOCATION_MATCH end, Relatives),
    case Matches of
        [] ->
            {Neighbours, _} = lists:partition(fun({Class, _}) -> Class =:= ?LOCATION_SUBSETEQ_LEFT end, Others),
            NextTable = lists:foldl(
                fun(Current, Accum) ->
                    case lists:member(Current, Candidates) of
                        true ->
                            Info = maps:get(Current, Accum),
                            NextStatus = variable_status_merge(Info#varinfo.status, Status),
                            maps:put(Current, Info#varinfo{ status = NextStatus}, Accum);
                        _ ->
                            Accum
                    end
                end,
                Table,
                util:list_usort_flatten([Names || {_, Names} <- Neighbours])
            ),
            general_update_status(NextTable, Status, [], Candidates, Visited);
        _ ->
            MatchesNames = util:list_usort_flatten([Names || {_, Names} <- Matches]),
            NextNames = [Name || Name <- MatchesNames, lists:member(Name, Candidates)],
            NextInfo = [maps:get(Name, Table) || Name <- NextNames],
            {NextStatus, NextRelatives} = general_collect_data(NextInfo, Status, []),
            general_update_status(Table, NextStatus, util:list_usort_append(Others, NextRelatives),
                Candidates -- NextNames, util:list_usort_append(Visited, NextNames))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_update_status/5'.
%% @end
%%-------------------------------------------------------------------------------------------
general_collect_data([], Status, Relatives) ->
    {Status, util:list_usort_flatten(Relatives)};
general_collect_data([Victim | Victims], Status, Relatives) ->
    general_collect_data(
        Victims,
        variable_status_merge(Status, Victim#varinfo.status),
        util:list_append(Relatives, Victim#varinfo.relatives)
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun constraint_variables_default/3'.
%% @end
%%-------------------------------------------------------------------------------------------
constraint_get_location(Class, SubseteqValue) ->
    case Class of
        ?CONSTRAINT_MATCH -> ?LOCATION_MATCH;
        ?CONSTRAINT_SUBSETEQ -> SubseteqValue;
        _ -> ?LOCATION_CONSTRAINT
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the free variables' information inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
free_variables(Value) ->
    Variables = maps:to_list(get_variables(Value)),
    FreeVariables = get_free_variables(Value),
    VS = [{K, V} || {K, V} <- Variables, lists:member(K, FreeVariables)],
    maps:from_list(VS).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the non free variables' information inside a type.
%% @param Value The value to analyze.
%% @returns A map with the information obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
non_free_variables(Value) ->
    Variables = maps:to_list(get_variables(Value)),
    FreeVariables = get_free_variables(Value),
    VS = [{K, V} || {K, V} <- Variables, not lists:member(K, FreeVariables)],
    maps:from_list(VS).

%%===========================================================================================
%% Height Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variable names inside a type or constraint.
%% @param Value The value to analyze.
%% @returns A list with the names obtained from the value.
%% @end
%%-------------------------------------------------------------------------------------------
get_height(Value) ->
    util:list_max(type:map(Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_PAIR, RT, []} ->
                    RT;
                {?TYPE_PAIR, RT, RCS} ->
                    max(RT, util:list_max(RCS));
                {?TYPE_SET, _, []} ->
                    0;
                {?TYPE_SET, _, RPS} ->
                    util:list_max(RPS) + 1;
                {?TYPE_SCHEME, _, RI} ->
                    RI;
                {?TYPE_LAMBDA, RPS, RR} ->
                    max(util:list_max(RPS), RR) + 1;
                {?TYPE_CONDITION, RT, []} ->
                    RT;
                {?TYPE_CONDITION, RT, RCS} ->
                    max(RT, util:list_max(RCS));
                {?TYPE_UNION, _, RTS} ->
                    util:list_max(RTS);
                {?CONSTRAINT_MATCH, RL, RR} ->
                    max(RL, RR);
                {?CONSTRAINT_SUBSETEQ, RL, RR} ->
                    max(RL, RR);
                _ ->
                    0
            end
        end
    )).

%%===========================================================================================
%% Utility Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Transforms a collection of records into a simple tuple.
%% @param Victim The record or records to transform.
%% @returns The new collection with simple values.
%% @end
%%-------------------------------------------------------------------------------------------
varinfo_to_simple(Victim) when is_record(Victim, varinfo) ->
    {Victim#varinfo.count, Victim#varinfo.status};
varinfo_to_simple(Victim) when is_map(Victim) ->
    maps:from_list([{K, varinfo_to_simple(V)} || {K, V} <- maps:to_list(Victim)]);
varinfo_to_simple(Victim) when is_list(Victim) ->
    [varinfo_to_simple(V) || V <- Victim];
varinfo_to_simple(Victim) ->
    Victim.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a couple of status.
%% @param Left The left status.
%% @param Right The right status.
%% @returns The merged status.
%% @end
%%-------------------------------------------------------------------------------------------
variable_status_merge(?VARIABLE_SINGLE, _) -> ?VARIABLE_SINGLE;
variable_status_merge(_, ?VARIABLE_SINGLE) -> ?VARIABLE_SINGLE;
variable_status_merge(_, _) -> ?VARIABLE_PLURAL.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Merges a couple of locations.
%% @param Left The left location.
%% @param Right The right location.
%% @returns The merged location.
%% @end
%%-------------------------------------------------------------------------------------------
variable_location_merge(Location, Location) -> Location;
variable_location_merge(?LOCATION_SINGLETON, _) -> ?LOCATION_SINGLETON;
variable_location_merge(_, ?LOCATION_SINGLETON) -> ?LOCATION_SINGLETON;
variable_location_merge(?LOCATION_CONSTRAINT, _) -> ?LOCATION_CONSTRAINT;
variable_location_merge(_, ?LOCATION_CONSTRAINT) -> ?LOCATION_CONSTRAINT;
variable_location_merge(_, _) -> ?LOCATION_NORMAL.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Merges a couple of variable info data.
%% @param Left The left data.
%% @param Right The right data.
%% @returns The merged data.
%% @end
%%-------------------------------------------------------------------------------------------
variable_data_merge(Left, Right) ->
    case Left#varinfo.name =/= Right#varinfo.name of
        true ->
            throw(not_supported);
        _ ->
            #varinfo{
                name = Left#varinfo.name,
                count = Left#varinfo.count + Right#varinfo.count,
                status = variable_status_merge(Left#varinfo.status, Right#varinfo.status),
                location = variable_location_merge(Left#varinfo.location, Right#varinfo.location),
                relatives = util:list_usort_append(Left#varinfo.relatives, Right#varinfo.relatives)
            }
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets a value inside a collection of variable info records.
%% @param Victim The record or records to query.
%% @param OnGet The on get function.
%% @returns The value or values inside.
%% @end
%%-------------------------------------------------------------------------------------------
variable_get_value(Victim, OnGet) when is_record(Victim, varinfo) ->
    OnGet(Victim);
variable_get_value(Victims, OnGet) when is_list(Victims) ->
    [variable_get_value(Value, OnGet) || Value <- util:list_flatten(Victims)];
variable_get_value(Victims, OnGet) when is_map(Victims) ->
    [variable_get_value(Value, OnGet) || {_, Value} <- maps:to_list(Victims)];
variable_get_value(Victim, _) ->
    Victim.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Sets a value inside a collection of variable info records.
%% @param Victim The record or records to change.
%% @param OnSet The on set function.
%% @returns The new changed value made.
%% @end
%%-------------------------------------------------------------------------------------------
variable_set_value(Victim, OnSet) when is_record(Victim, varinfo) ->
    OnSet(Victim);
variable_set_value(Victims, OnSet) when is_list(Victims) ->
    [variable_set_value(Value, OnSet) || Value <- util:list_flatten(Victims)];
variable_set_value(Victim, _) ->
    Victim.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the names inside a collection of records.
%% @param Victim The record or records to query.
%% @returns The value or values inside.
%% @end
%%-------------------------------------------------------------------------------------------
variable_get_name(Victim) ->
    variable_get_value(Victim, fun(Item) -> Item#varinfo.name end).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Sets the status inside a collection of records.
%% @param Victim The record or records to change.
%% @param Status The new status value to set.
%% @returns The new changed value made.
%% @end
%%-------------------------------------------------------------------------------------------
variable_set_status(Victim, Status) ->
    variable_set_value(Victim, fun(Item) -> Item#varinfo{ status = Status } end).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Sets the location inside a collection of records.
%% @param Victim The record or records to change.
%% @param Location The new location value to set.
%% @returns The new changed value made.
%% @end
%%-------------------------------------------------------------------------------------------
variable_set_location(Victim, Location) ->
    variable_set_value(Victim, fun(Item) -> Item#varinfo{ location = Location } end).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Sets the location inside a collection of records.
%% @param Victim The record or records to change.
%% @param Location The new location value to set.
%% @returns The new changed value made.
%% @end
%%-------------------------------------------------------------------------------------------
variable_add_relatives_and_location(Victim, Location, Relatives) ->
    variable_set_value(
        Victim,
        fun(Item) ->
            case Relatives of
                {_, []} ->
                    Item#varinfo{ location = Location };
                _ ->
                    Item#varinfo{
                        location = Location,
                        relatives = util:list_usort_append(Item#varinfo.relatives, Relatives)
                    }
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Relax the status inside a collection of records.
%% @param Victim The record or records to change.
%% @returns The new changed value made.
%% @end
%%-------------------------------------------------------------------------------------------
variable_status_relax(Victim) ->
    variable_set_value(
        Victim,
        fun(Item) ->
            case Item#varinfo.location of
                ?LOCATION_SINGLETON -> Item;
                _ -> Item#varinfo{ status = ?VARIABLE_PLURAL }
            end
        end
    ).
