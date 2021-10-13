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
%%%| @copyright (C) 2019-2020, Gorka Suárez García                                          |
%%%|========================================================================================|
-module(identity).
-author("Gorka Suárez García").
-export([type/1]).
-include("type.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% The identity function for types.
%% @end
%%-------------------------------------------------------------------------------------------
type(Value) ->
    case Value of
        Value when is_list(Value) ->
            [type(Item) || Item <- Value];
        {?TYPE_PAIR, Type, Constraints} ->
            type:pair(type(Type), type(Constraints));
        {?TYPE_VARIABLE, Name, Linked} ->
            type:variable(Name, Linked);
        {?TYPE_LITERAL, Literal} ->
            type:literal(Literal);
        {?TYPE_SET, Name, Parameters} ->
            type:set(Name, type(Parameters));
        {?TYPE_SCHEME, Variables, Inner} ->
            type:scheme(type(Variables), type(Inner));
        {?TYPE_LAMBDA, Parameters, Result} ->
            type:lambda(type(Parameters), type(Result));
        {?TYPE_CONDITION, Type, Constraints} ->
            type:condition(type(Type), type(Constraints));
        {?TYPE_UNION, Ordered, Types} ->
            type:union_or_sequence(Ordered, type(Types));
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint:match(type(Variable), type(Type));
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            constraint:subseteq(type(LeftType), type(RightType));
        {?CONSTRAINT_JOINABLE, Variables} ->
            constraint:joinable(type(Variables));
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            type:symbol_call(type(Variable), type(Parameters));
        Otherwise ->
            Otherwise
    end.
