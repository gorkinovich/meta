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
-module(environment).
-author("Gorka Suárez García").
-export([new/0, get/2, set/3, append/3, merge/2, rename/2, copy_with_aliases/2]).
-include("type.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an empty environment.
%% @returns The new environment.
%% @end
%%-------------------------------------------------------------------------------------------
new() ->
    maps:new().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type for a variable inside an environment.
%% @param Table The environment.
%% @param Name The name of the variable.
%% @returns The type related to the variable.
%% @end
%%-------------------------------------------------------------------------------------------
get(Table, Name) when is_map(Table) ->
    maps:get(Name, Table, type:any());
get(_, _) ->
    type:none().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the type for a variable inside an environment.
%% @param Table The environment.
%% @param Name The name of the variable.
%% @returns The changed environment.
%% @end
%%-------------------------------------------------------------------------------------------
set(Table, Name, Type) when is_map(Table) ->
    maps:put(Name, Type, Table);
set(Victim, _, _) ->
    Victim.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the type for a variable inside an environment.
%% @param Table The environment.
%% @param Name The name of the variable.
%% @returns The changed environment.
%% @end
%%-------------------------------------------------------------------------------------------
append(Table, Name, Type) when is_map(Table) ->
    Value = maps:get(Name, Table, []),
    NextValue = util:list_append(Value, Type),
    maps:put(Name, NextValue, Table);
append(Victim, _, _) ->
    Victim.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a couple of environments.
%% @param Left The left environment.
%% @param Right The right environment.
%% @returns The merged environment.
%% @end
%%-------------------------------------------------------------------------------------------
merge(Left, Right) when is_map(Left), is_map(Right) ->
    lists:foldl(
        fun({Name, Type}, Table) ->
            List = util:list_append(Type, get(Table, Name)),
            set(Table, Name, poly:infimum(util:list_usort_flatten(List)))
        end,
        Left,
        maps:to_list(Right)
    );
merge(?CONSTRAINTS_BOTTOM, _) ->
    ?CONSTRAINTS_BOTTOM;
merge(_, ?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
merge(_, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a couple of environments.
%% @param Left The left environment.
%% @param Right The right environment.
%% @returns The merged environment.
%% @end
%%-------------------------------------------------------------------------------------------
rename(Table, Names) ->
    Values = maps:to_list(Table),
    NextValues = [{util:substitution(Name, Names),
        type:rename_free_variables(Type, Names)} || {Name, Type} <- Values],
    lists:foldl(
        fun({Key, Value}, Accum) ->
            append(Accum, Key, Value)
        end,
        new(),
        NextValues
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the types for some variables inside an environment using a set of aliases.
%% @param Table The environment.
%% @param Aliases The aliases to use.
%% @returns The changed environment.
%% @end
%%-------------------------------------------------------------------------------------------
copy_with_aliases(Table, Aliases) when is_map(Table) ->
    case get_alias_candidate(maps:keys(Table), Aliases, []) of
        {Origin, Destination, NextAliases} ->
            NextTable = set(Table, Destination, get(Table, Origin)),
            copy_with_aliases(NextTable, NextAliases);
        {infimum, Origin, Destination, NextAliases} ->
            OriginType = get(Table, Origin),
            DestinationType = get(Table, Destination),
            NextType = mono:infimum(OriginType, DestinationType),
            NextTable = set(Table, Destination, NextType),
            copy_with_aliases(NextTable, NextAliases);
        _ ->
            Table
    end;
copy_with_aliases(Table, _) ->
    Table.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun copy_with_aliases/2'.
%% @end
%%-------------------------------------------------------------------------------------------
get_alias_candidate(_, [], _) ->
    nothing;
get_alias_candidate(Names, [Alias | Aliases], Previous) ->
    LeftName = type:get_variable_name(constraint:get_left(Alias)),
    RightName = type:get_variable_name(constraint:get_right(Alias)),
    LeftTest = lists:member(LeftName, Names),
    RightTest = lists:member(RightName, Names),
    case {LeftTest, RightTest} of
        {true, true} ->
            {infimum, LeftName, RightName, Aliases ++ Previous};
        {true, false} ->
            {LeftName, RightName, Aliases ++ Previous};
        {false, true} ->
            {RightName, LeftName, Aliases ++ Previous};
        {false, false} ->
            get_alias_candidate(Names, Aliases, [Alias | Previous])
    end.
