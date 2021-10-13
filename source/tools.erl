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
-module(tools).
-author("Gorka Suárez García").
-export([
    update_status/3, update_status_freevars/2, update_status_dirty/3,
    is_func_any/1, is_func_none/1, get_call_type/3, make_subseteq_constraint/3,
    make_match_constraint/3, check_and_make_table_constraints/3, make_shadow_type/1
]).
-include("reduce.hrl").

%%===========================================================================================
%% Utility Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Updates inside a status the type for a definition.
%% @end
%%-------------------------------------------------------------------------------------------
update_status(Name, Table, Status) ->
    DefType = table:get_definition_type(Table, Name),
    NextEnv = util:map_set(Status#status.environment, Name, DefType),
    Status#status{ environment = NextEnv }.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Updates the free variables inside a status data with the free variables of a type.
%% @end
%%-------------------------------------------------------------------------------------------
update_status_freevars(Status, Type) ->
    FVS = query:get_free_variables(Type),
    Status#status{ freevars = util:list_usort_append(Status#status.freevars, FVS) }.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Updates the dirty definition names inside a status data.
%% @end
%%-------------------------------------------------------------------------------------------
update_status_dirty(Status, Caller, Names) ->
    FinalNames = lists:delete(Caller, Names),
    Status#status{ dirty = lists:usort(Status#status.dirty ++ FinalNames) }.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type is an any function.
%% @end
%%-------------------------------------------------------------------------------------------
is_func_any(Type) when is_tuple(Type) ->
    type:is_any(Type) orelse type:is_lambda_any(Type);
is_func_any(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type is a none function.
%% @end
%%-------------------------------------------------------------------------------------------
is_func_none(Type) when is_tuple(Type) ->
    type:is_none(Type) orelse type:is_lambda_none(Type);
is_func_none(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type of a called function.
%% @end
%%-------------------------------------------------------------------------------------------
get_call_type(Status, Name, Size) ->
    case language:is_bif(Name) of
        true ->
            language:get_bif_type(Name, Size);
        false ->
            Environment = Status#status.environment,
            TypeBySize = util:map_get(Environment, {Name, Size}, null),
            case type:is_type(TypeBySize) of
                true ->
                    TypeBySize;
                _ ->
                    Type = util:map_get(Environment, Name, null),
                    case {type:is_type(Type), Type} of
                        {true, _} -> Type;
                        {_, none} -> type:lambda_none(Size);
                        {_, _} -> type:lambda_any(Size)
                    end
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes a subset or equal constraint for the new fresh type variables introduced.
%% @end
%%-------------------------------------------------------------------------------------------
make_subseteq_constraint(LinkedVars, Left, Right) ->
    make_generic_constraint(LinkedVars, Left, Right, fun constraint:subseteq/2).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes a match constraint for the new fresh type variables introduced.
%% @end
%%-------------------------------------------------------------------------------------------
make_match_constraint(LinkedVars, Left, Right) ->
    make_generic_constraint(LinkedVars, Left, Right, fun constraint:match/2).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a constraint for the new fresh type variables introduced.
%% @end
%%-------------------------------------------------------------------------------------------
make_generic_constraint(LinkedVars, Left, Right, Ctor) ->
    IsLinked = fun(Victim) -> lists:member(Victim, LinkedVars) end,
    Ctor(type:variable(Left, IsLinked(Left)), type:variable(Right, IsLinked(Right))).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Takes a set of constraints to split them into a map of categories.
%% @end
%%-------------------------------------------------------------------------------------------
check_and_make_table_constraints(Constraints, OnSuccess, OnBottom) ->
    {Table, _} = table:split_constraints(Constraints),
    {T2, MonoConditions} = table:get_and_remove_mono_conditions(Table),
    case mono:is_bottom(MonoConditions) of
        true -> OnBottom(T2);
        _ -> OnSuccess(T2)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes a shadow of a given type.
%% @end
%%-------------------------------------------------------------------------------------------
make_shadow_type(Victim) ->
    FVS = query:get_free_variables(Victim),
    Table = data:get_rename_table(FVS),
    NCS = [constraint:subseteq(type:variable(maps:get(V, Table)), type:variable(V)) || V <- FVS],
    NRB = type:rename(Victim, Table),
    normalize:recursive(type:condition(NRB, NCS)).
