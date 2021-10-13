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
-module(table).
-author("Gorka Suárez García").
-export([
    % Properties Functions:
    get_moves/1, set_moves/2, set_conditions/2, get_conditions/1, get_definition/2,
    set_definition/2, get_definition_constraints/2, set_definition_constraints/3,
    get_definition_type/2,

    % Definitions Functions:
    get_definitions/1, get_definitions/2, set_definitions/2,

    % Dependencies Functions:
    get_dependencies/1, sort_dependencies/1,

    % Conditions Functions:
    get_and_remove_mono_conditions/1, remove_mono_conditions/1,

    % Constraints Functions:
    get_constraints/1, split_constraints/1, split_constraints/2,

    % Operator Functions:
    equal_definition/3, equal_definitions/3,

    % Check Functions:
    is_bottom/1
]).
-include("type.hrl").

%|--------------------|-------------------------------------|
%| Table keys         | Table values                        |
%|--------------------|-------------------------------------|
%| moves              | Constraints                         |
%| conditions         | Constraints                         |
%| {definition, Name} | {Name, Constraints, Dependencies}   |
%|--------------------|-------------------------------------|

%%===========================================================================================
%% Macros
%%===========================================================================================

-define(KEY_MOVES,      moves).
-define(KEY_CONDITIONS, conditions).
-define(KEY_DEFINITION, definition).

%%===========================================================================================
%% Properties Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the constraints to move inside a map of categories.
%% @param Table The map of categories of constraints.
%% @returns The set of constraints to move inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_moves(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
get_moves(Table) ->
    maps:get(?KEY_MOVES, Table, []).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the constraints to move inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Constraints The set of constraints value to set.
%% @returns The modified map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
set_moves(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
set_moves(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
set_moves(Table, Constraints) ->
    maps:put(?KEY_MOVES, Constraints, Table).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the constraint conditions inside a map of categories.
%% @param Table The map of categories of constraints.
%% @returns The set of constraint conditions inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_conditions(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
get_conditions(Table) ->
    maps:get(?KEY_CONDITIONS, Table, []).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the constraint conditions inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Constraints The set of constraints value to set.
%% @returns The modified map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
set_conditions(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
set_conditions(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
set_conditions(Table, Constraints) ->
    maps:put(?KEY_CONDITIONS, Constraints, Table).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a type variable definition inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Name The name of the definition to get.
%% @returns The type variable definition inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition(?CONSTRAINTS_BOTTOM, Name) ->
    {Name, ?CONSTRAINTS_BOTTOM, []};
get_definition(Table, Name) ->
    maps:get({?KEY_DEFINITION, Name}, Table, {Name, [], []}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets a type variable definition inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Value The value of definition to set.
%% @returns The modified map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
set_definition(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
set_definition(_, {_, ?CONSTRAINTS_BOTTOM, _}) ->
    ?CONSTRAINTS_BOTTOM;
set_definition(Table, {Name, Constraints, Dependencies}) ->
    maps:put({?KEY_DEFINITION, Name}, {Name, Constraints, Dependencies}, Table);
set_definition(Table, _) ->
    Table.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the constraints for a type variable definition inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Name The name of the definition to get.
%% @returns The constraints for a type variable definition.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition_constraints(Table, Name) ->
    {_, Constraints, _} = get_definition(Table, Name),
    Constraints.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the constraints for a type variable definition inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Name The name of the definition to get.
%% @param Constraints The set of constraints value to set.
%% @returns The modified map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
set_definition_constraints(?CONSTRAINTS_BOTTOM, _, _) ->
    ?CONSTRAINTS_BOTTOM;
set_definition_constraints(_, _, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
set_definition_constraints(Table, Name, Constraints) ->
    {_, _, Dependencies} = get_definition(Table, Name),
    set_definition(Table, {Name, Constraints, Dependencies}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type for a type variable inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Name The name of the definition to get.
%% @returns The type of the definitions inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_definition_type(?CONSTRAINTS_BOTTOM, _) ->
    type:none();
get_definition_type(Table, Name) ->
    DCS = get_definition_constraints(Table, Name),
    case constraint:get_definition(DCS, Name, plastic_love) of
        plastic_love -> constraint:get_definition(get_moves(Table), Name);
        Type -> Type
    end.

%%===========================================================================================
%% Definitions Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type variable definitions inside a map of categories.
%% @param Table The map of categories of constraints.
%% @returns The type variable definitions inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_definitions(?CONSTRAINTS_BOTTOM) ->
    [];
get_definitions(Table) ->
    lists:usort(lists:filter(
        fun(Victim) ->
            Victim =/= none
        end,
        lists:map(
            fun(Victim) ->
                case Victim of
                    {{?KEY_DEFINITION, _}, Value} ->
                        Value;
                    _ ->
                        none
                end
            end,
            maps:to_list(Table)
        )
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets some type variable definitions inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Names The names of the definitions to get.
%% @returns The type variable definitions inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_definitions(Table, Names) ->
    lists:foldl(
        fun(Name, Accum) ->
            [get_definition(Table, Name) | Accum]
        end,
        [],
        Names
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a type variable definition inside a map of categories.
%% @param Table The map of categories of constraints.
%% @param Values The values of definitions to set.
%% @returns The modified map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
set_definitions(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
set_definitions(Table, Values) ->
    lists:foldl(
        fun(Value, Accum) ->
            set_definition(Accum, Value)
        end,
        Table,
        Values
    ).

%%===========================================================================================
%% Dependencies Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets all the dependencies inside a map of categories.
%% @param Table The map of categories of constraints.
%% @returns The set of dependencies inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_dependencies(?CONSTRAINTS_BOTTOM) ->
    [];
get_dependencies(Table) ->
    [{Name, DS} || {Name, _, DS} <- get_definitions(Table)].

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sorts a list of dependencies in a sequence of dependencies.
%% @param Candidates The list of dependencies.
%% @returns The sequence of dependencies obtained.
%% @end
%%-------------------------------------------------------------------------------------------
sort_dependencies(Candidates) ->
    {Root, NextCandidates} = lists:partition(
        fun(Victim) ->
            case Victim of
                {_, []} -> true;
                _ -> false
            end
        end,
        Candidates
    ),
    Names = get_candidates_names(Root),
    Tail = sort_dependencies(NextCandidates, Names),
    case Names of
        [] -> Tail;
        _ -> [{root, Names} | Tail]
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Sorts a list of dependencies in a sequence of dependencies.
%% @param Candidates The list of dependencies.
%% @param SuperNames The names of the previous nodes.
%% @returns The sequence of dependencies obtained.
%% @end
%%-------------------------------------------------------------------------------------------
sort_dependencies([], _) ->
    [];
sort_dependencies(Candidates, SuperNames) ->
    % Get those candidates with only dependencies with the previous names:
    {Node, NextCandidates} = lists:partition(
        fun({_, DS}) ->
            util:list_all_member(DS, SuperNames)
        end,
        Candidates
    ),
    % The the result of the dependencies partition:
    case Node of
        [] ->
            % When is empty the result, check if there are candidates with
            % recursive dependencies inside the current candidates:
            case find_recursive_dependencies(Candidates, SuperNames) of
                [] ->
                    [{unknown, get_candidates_names(Candidates)}];
                Names ->
                    % If we get a group or recursive dependencies, first we remove
                    % the candidates and then we get the next node of dependencies:
                    FinalCandidates = lists:filter(
                        fun({N, _}) ->
                            not lists:member(N, Names)
                        end,
                        NextCandidates
                    ),
                    [{recnode, Names} | sort_dependencies(FinalCandidates, SuperNames ++ Names)]
            end;
        _ ->
            % When we obtain a list of candidates, we make a new node and
            % go to the next iteration of the sort dependencies function:
            Names = [Name || {Name, _} <- Node],
            [{node, Names} | sort_dependencies(NextCandidates, SuperNames ++ Names)]
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Finds recursive dependencies inside a list of dependencies.
%% @param Candidates The list of candidates to check.
%% @param SuperNames The names of the previous nodes.
%% @returns The set of names of the recursive dependencies obtained.
%% @end
%%-------------------------------------------------------------------------------------------
find_recursive_dependencies([], _) ->
    [];
find_recursive_dependencies(Candidates, SuperNames) ->
    % Remove the supernames from the dependencies of the candidates:
    NextCandidates = lists:map(
        fun({N, DS}) ->
            {N, DS -- SuperNames}
        end,
        Candidates
    ),
    % Start looking for any recursive group of names:
    find_candidates_loop(NextCandidates).

%%===========================================================================================
%% Candidates Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the names inside a list of candidates.
%% @param Candidates The list of candidates to check.
%% @returns The names inside a list of candidates.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_names(Candidates) ->
    [Name || {Name, _} <- Candidates].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the depencencies inside a list of candidates.
%% @param Candidates The list of candidates to check.
%% @returns The depencencies inside a list of candidates.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_dependencies(Candidates) ->
    util:list_flatten([Dependencies || {_, Dependencies} <- Candidates]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the depencencies of a candidate inside a list of candidates.
%% @param Name The name of the candidate to check.
%% @param Candidates The list of candidates to check.
%% @returns The depencencies of the selected candidate.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_dependencies(Name, Candidates) ->
    util:list_flatten([CDS || {CN, CDS} <- Candidates, CN =:= Name]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a partition inside a list of candidates.
%% @param Name The name of the candidate to check.
%% @param Candidates The list of candidates to check.
%% @returns A tuple with a list with the cadidates with the selected
%%          name and the list with the rest of the candidates.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_partition(Name, Candidates) ->
    lists:partition(fun({N, _}) -> N =:= Name end, Candidates).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Finds if there is a loop of dependencies inside a list of candidates.
%% @param Candidates The list of candidates to check.
%% @returns A list with the names of the loop.
%% @end
%%-------------------------------------------------------------------------------------------
find_candidates_loop(Candidates) ->
    find_candidates_loop(get_candidates_names(Candidates), Candidates).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Finds if there is a loop of dependencies inside a list of candidates.
%% @param Names The current names of candidates to check.
%% @param Candidates The list of candidates to check.
%% @returns A list with the names of the loop.
%% @end
%%-------------------------------------------------------------------------------------------
find_candidates_loop([], _) ->
    [];
find_candidates_loop([Name | Names], Candidates) ->
    case get_candidates_loop(Name, Candidates) of
        {LoopNames, LoopDependencies} ->
            case LoopDependencies of
                [] -> LoopNames;
                _ -> find_candidates_loop(Names, Candidates)
            end;
        _ ->
            find_candidates_loop(Names, Candidates)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the loop of dependencies inside a list of candidates.
%% @param Name The current name of the candidate to check.
%% @param Candidates The list of candidates to check.
%% @returns A tuple with a list with the names of the loop and
%%          the dependencies of that loop.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_loop(Name, Candidates) ->
    case check_candidates_autoloop(Name, Candidates) of
        true ->
            {[Name], []};
        false ->
            {Current, Next} = get_candidates_partition(Name, Candidates),
            Dependencies = get_candidates_dependencies(Current),
            get_candidates_loop(Name, Dependencies, [Name], Dependencies, Current, Next)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the loop of dependencies inside a list of candidates.
%% @param Goal The final name inside the loop.
%% @param Names The names to visit to check the loop.
%% @param CurrentNames The names of the current loop.
%% @param CurrentDependencies The dependencies of the current loop.
%% @param Visited The visited list of candidates.
%% @param Candidates The not visited list of candidates.
%% @returns A tuple with a list with the names of the loop and
%%          the dependencies of that loop.
%% @end
%%-------------------------------------------------------------------------------------------
get_candidates_loop(_, [], _, _, _, _) ->
    loop_not_found;
get_candidates_loop(Goal, [Goal | Names], CurrentNames, CurrentDependencies, Visited, Candidates) ->
    Dependencies = get_candidates_dependencies(Candidates),
    case lists:member(Goal, Dependencies) of
        true ->
            get_candidates_loop(Goal, Names, CurrentNames, CurrentDependencies, Visited, Candidates);
        false ->
            make_candidates_loop(CurrentNames, CurrentDependencies)
    end;
get_candidates_loop(Goal, [Name | Names], CurrentNames, CurrentDependencies, Visited, Candidates) ->
    case lists:member(Name, CurrentNames) of
        true ->
            get_candidates_loop(Goal, Names, CurrentNames, CurrentDependencies, Visited, Candidates);
        false ->
            {Current, NextCandidates} = get_candidates_partition(Name, Candidates),
            Dependencies = get_candidates_dependencies(Current),
            FinalNames = util:list_usort([Name | CurrentNames]),
            FinalDependencies = util:list_usort_append(CurrentDependencies, Dependencies),
            NextVisited = util:list_usort_append(Visited, Current),
            NextNames = util:list_usort_append(Names, Dependencies),
            get_candidates_loop(Goal, NextNames, FinalNames, FinalDependencies, NextVisited, NextCandidates)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes the data of a loop of dependencies inside a list of candidates.
%% @param CurrentNames The names of the current loop.
%% @param CurrentDependencies The dependencies of the current loop.
%% @returns A tuple with a list with the names of the loop and
%%          the dependencies of that loop.
%% @end
%%-------------------------------------------------------------------------------------------
make_candidates_loop(CurrentNames, CurrentDependencies) ->
    FinalNames = util:list_usort_flatten(CurrentNames),
    FinalDependencies = util:list_usort_flatten(CurrentDependencies) -- FinalNames,
    {FinalNames, FinalDependencies}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a candidate is an autoloop inside a list of candidates.
%% @param Name The current name of the candidate to check.
%% @param Candidates The list of candidates to check.
%% @param
%% @returns .
%% @end
%%-------------------------------------------------------------------------------------------
check_candidates_autoloop(Name, Candidates) ->
    case get_candidates_dependencies(Name, Candidates) of
        [Name] -> true;
        _ -> false
    end.

%%===========================================================================================
%% Conditions Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Takes map of categories and obtains the monomorphic conditions.
%% @param Table The map of categories of constraints.
%% @returns A tuple which first field is the new map without the obtained monomorphic
%%          conditions, and the second field the obtained monomorphic conditions.
%% @end
%%-------------------------------------------------------------------------------------------
get_and_remove_mono_conditions(?CONSTRAINTS_BOTTOM) ->
    {?CONSTRAINTS_BOTTOM, []};
get_and_remove_mono_conditions(Table) ->
    {MonoConditions, PolyConditions} = lists:partition(
        fun({?CONSTRAINT_SUBSETEQ, Left, Right}) ->
            type:is_monomorphic(Left) andalso type:is_monomorphic(Right)
        end,
        get_conditions(Table)
    ),
    {maps:put(?KEY_CONDITIONS, PolyConditions, Table), MonoConditions}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Takes map of categories and removes the monomorphic conditions.
%% @param Table The map of categories of constraints.
%% @returns The new map without the monomorphic conditions.
%% @end
%%-------------------------------------------------------------------------------------------
remove_mono_conditions(Table) ->
    {Result, _} = get_and_remove_mono_conditions(Table),
    Result.

%%===========================================================================================
%% Constraints Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets all the constraints inside a map of categories.
%% @param Table The map of categories of constraints.
%% @returns The set of constraints inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
get_constraints(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
get_constraints(Table) ->
    util:list_usort_flatten(lists:map(
        fun(Victim) ->
            case Victim of
                {?KEY_MOVES, CS} -> CS;
                {?KEY_CONDITIONS, CS} -> CS;
                {{?KEY_DEFINITION, _}, {_, CS, _}} -> CS;
                _ -> throw(not_supported)
            end
        end,
        maps:to_list(Table)
    )).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Takes a set of constraints to split them into a map of categories.
%% @param Constraints The set of constraints to take.
%% @returns The constraints inside a map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
split_constraints(?CONSTRAINTS_BOTTOM) ->
    {?CONSTRAINTS_BOTTOM, []};
split_constraints(Constraints) ->
    split_constraints(maps:new(), Constraints, []).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Takes a set of constraints to split them into a map of categories.
%% @param Table The map of categories of constraints.
%% @param Constraints The set of constraints to take.
%% @returns The constraints inside a map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
split_constraints(?CONSTRAINTS_BOTTOM, _) ->
    {?CONSTRAINTS_BOTTOM, []};
split_constraints(_, ?CONSTRAINTS_BOTTOM) ->
    {?CONSTRAINTS_BOTTOM, []};
split_constraints(Table, Constraints) ->
    split_constraints(Table, Constraints, []).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Takes a set of constraints to split them into a map of categories.
%% @param Table The map of categories of constraints.
%% @param Constraints The set of constraints to take.
%% @param Names The current list of definition names.
%% @returns The constraints inside a map of categories and a list of the
%%          definition names inside the map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
split_constraints(Table, [], Names) ->
    {Table, lists:usort(Names)};
split_constraints(Table, [Constraint | Constraints], Names) ->
    case Constraint of
        % If the left side of the constraint is a type variable we have a definition:
        {Class, {?TYPE_VARIABLE, Name, Linked}, Right} when
            Class =:= ?CONSTRAINT_MATCH; Class =:= ?CONSTRAINT_SUBSETEQ ->
            {Name, CS, DS} = get_definition(Table, Name),
            CS2 = constraint:insert(CS, {Class, type:variable(Name, Linked), Right}),
            DS2 = lists:usort(DS ++ query:get_call_name_variables(Right)),
            NextTable = set_definition(Table, {Name, CS2, DS2}),
            split_constraints(NextTable, Constraints, [Name | Names]);

        % If the left side of the constraint isn't a type variable, then the constraint
        % is a condition to be check if the final set of constraints is bottom or not:
        {?CONSTRAINT_SUBSETEQ, Left, Right} ->
            NextValue = constraint:insert(get_conditions(Table), constraint:subseteq(Left, Right)),
            split_constraints(maps:put(?KEY_CONDITIONS, NextValue, Table), Constraints, Names);

        % Joinable constraints can't be reduced, therefore they'll move into the final
        % set of constraints after the reduce transformation:
        {?CONSTRAINT_JOINABLE, Variables} ->
            NextValue = constraint:insert(get_moves(Table), constraint:joinable(Variables)),
            split_constraints(maps:put(?KEY_MOVES, NextValue, Table), Constraints, Names);

        % Other constraint cases are not supported:
        _ ->
            throw(not_supported)
    end.

%%===========================================================================================
%% Operator Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a definition is equal in two tables.
%% @param Name The name of the definition to check.
%% @param LeftTable The left map of categories of constraints.
%% @param RightTable The right map of categories of constraints.
%% @returns 'true' when the definitions are the same, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
equal_definition(Name, LeftTable, RightTable) ->
    LDef = get_definition(LeftTable, Name),
    RDef = get_definition(RightTable, Name),
    LDef =:= RDef.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if some definitions are equal in two tables.
%% @param Names The names of the definitions to check.
%% @param LeftTable The left map of categories of constraints.
%% @param RightTable The right map of categories of constraints.
%% @returns 'true' when the definitions are the same, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
equal_definitions([], _, _) ->
    true;
equal_definitions([Name | Names], LeftTable, RightTable) ->
    case equal_definition(Name, LeftTable, RightTable) of
        true -> equal_definitions(Names, LeftTable, RightTable);
        _ -> false
    end.

%%===========================================================================================
%% Check Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a map of categories is bottom.
%% @param Table The map of categories of constraints.
%% @returns Returns 'true' when is bottom.
%% @end
%%-------------------------------------------------------------------------------------------
is_bottom(Table) ->
    Constraints = get_constraints(Table),
    case ground:get_environment(Constraints) of
        {_, ?CONSTRAINTS_BOTTOM, _} -> true;
        _ -> false
    end.
