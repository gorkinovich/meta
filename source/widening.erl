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
-module(widening).
-author("Gorka Suárez García").
-export([execute/3, change_type/1]).
-include("type.hrl").
-include("reduce.hrl").

%%===========================================================================================
%% Constant Values
%%===========================================================================================

-define(WIDENING_DEPTH, 3).
-define(MAX_TYPE_DEPTH, 4).

%%===========================================================================================
%% Widening Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Changes a map of categories of constraints to apply a widening process.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
execute(Names, Table, Status) ->
    NextTable = change_table(Names, Table),
    NextStatus = change_status(Names, Status),
    {NextTable, NextStatus}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a table to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_table(Names, Table) ->
    lists:foldl(
        fun(Name, Accum) ->
            Constraints = table:get_definition_constraints(Accum, Name),
            NextConstraints = change_constraints(Constraints),
            table:set_definition_constraints(Accum, Name, NextConstraints)
        end,
        Table,
        Names
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a status to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_status(Names, Status) ->
    Status#status{ environment = change_environment(Names, Status#status.environment) }.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes an environment to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_environment(Names, Environment) ->
    lists:foldl(
        fun(Name, Accum) ->
            Type = util:map_get(Accum, Name, type:any()),
            NextType = change_type(Type),
            util:map_set(Accum, Name, NextType)
        end,
        Environment,
        Names
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_constraints(?CONSTRAINTS_BOTTOM) ->
    ?CONSTRAINTS_BOTTOM;
change_constraints([]) ->
    [];
change_constraints([Constraint | Constraints]) ->
    [change_constraint(Constraint) | change_constraints(Constraints)].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a set of constraints to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_constraint(Constraint) ->
    case Constraint of
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint:match(Variable, change_type(Type));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            constraint:subseteq(LeftType, change_type(RightType));
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Changes a type to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_type(Value) ->
    NextValue = change_type(Value, query:get_height(Value) - 1),
    normalize:recursive(NextValue).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Changes a type to apply a widening process.
%% @end
%%-------------------------------------------------------------------------------------------
change_type(Value, Height) when is_list(Value) ->
    [change_type(Item, Height) || Item <- Value];
change_type(Value, Height) when Height < 1 ->
    case type:is_lambda_any(Value) of
        true -> Value;
        _ -> type:any()
    end;
change_type(Value, Height) ->
    case Value of
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(change_type(Type, Height), change_type(Constraints, Height));
        {?TYPE_SET, Name, Parameters} ->
            case Name of
                ?TYPE_TUPLE ->
                    type:set(Name, change_type(Parameters, Height - 1));
                ?TYPE_NELIST ->
                    type:set(Name, change_type(Parameters, Height - 1));
                _ ->
                    type:set(Name, change_type(Parameters, Height))
            end;
        {?TYPE_SCHEME, Variables, Inner} ->
            type:scheme(Variables, change_type(Inner, Height));
        {?TYPE_LAMBDA, Parameters, Result} ->
            type:lambda(change_type(Parameters, Height - 1), change_type(Result, Height - 1));
        {?TYPE_CONDITION, Type, Constraints} ->
            type:condition(change_type(Type, Height), change_type(Constraints, Height));
        {?TYPE_UNION, Ordered, Types} ->
            type:union_or_sequence(Ordered, change_type(Types, Height));
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint:match(Variable, change_type(Type, Height));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            constraint:subseteq(LeftType, change_type(RightType, Height));
        Otherwise ->
            Otherwise
    end.
