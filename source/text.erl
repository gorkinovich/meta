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
-module(text).
-author("Gorka Suárez García").
-export([type/1]).
-include("type.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Converts a type into a string.
%% @param Value The value to convert.
%% @returns The value turn into a string.
%% @end
%%-------------------------------------------------------------------------------------------
type(Values) when is_list(Values) ->
    util:to_string(fun type/1, get_list_separator(Values), Values);
type(Value) ->
    type:map(Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_PAIR, RT, []} ->
                    RT;
                {?TYPE_PAIR, _, "#BOTTOM#"} ->
                    "none()";
                {?TYPE_PAIR, RT, RCS} ->
                    RT ++ " where " ++ util:join_strings(", ", RCS) ++ " end";
                {?TYPE_VARIABLE, Name, Linked} ->
                    case {Linked, data:get_show_linked()} of
                        {true, true} -> name(Name) ++ "*";
                        _ -> name(Name)
                    end;
                {?TYPE_LITERAL, Literal} ->
                    case Literal of
                        {} -> "{}";
                        [] -> "[]";
                        String when is_list(String) -> "\"" ++ String ++ "\"";
                        Atom when is_atom(Atom) -> "\'" ++ util:to_string(Atom) ++ "\'";
                        Number -> util:to_string(Number)
                    end;
                {?TYPE_SET, ?TYPE_TUPLE, []} ->
                    util:to_string(?TYPE_TUPLE) ++ "()";
                {?TYPE_SET, ?TYPE_TUPLE, RPS} ->
                    "{" ++ util:join_strings(", ", RPS) ++ "}";
                {?TYPE_SET, Name, RPS} ->
                    util:to_string(Name) ++ "(" ++ util:join_strings(", ", RPS) ++ ")";
                {?TYPE_SCHEME, [], RI} ->
                    RI;
                {?TYPE_SCHEME, RVS, RI} ->
                    "(forall " ++ util:to_string(fun name/1, ", ", RVS) ++ ": " ++ RI ++ ")";
                {?TYPE_LAMBDA, RPS, RR} ->
                    "(" ++ util:join_strings(", ", RPS) ++ ") -> " ++ RR;
                {?TYPE_CONDITION, _, "#BOTTOM#"} ->
                    "none()";
                {?TYPE_CONDITION, RT, []} ->
                    RT;
                {?TYPE_CONDITION, RT, RCS} ->
                    "(" ++ RT ++ " when " ++ util:join_strings(", ", RCS) ++ ")";
                {?TYPE_UNION, false, RTS} ->
                    util:join_strings(" + ", RTS);
                {?TYPE_UNION, true, RTS} ->
                    util:join_strings("; ", RTS);
                {?CONSTRAINT_MATCH, RL, RR} ->
                    RL ++ " := " ++ RR;
                {?CONSTRAINT_SUBSETEQ, RL, RR} ->
                    RL ++ " <= " ++ RR;
                {?CONSTRAINT_JOINABLE, RVS} ->
                    "<" ++ util:join_strings(", ", RVS) ++ ">";
                {?TYPE_SYMBOL_CALL, RV, RPS} ->
                    "@:" ++ RV ++ "(" ++ util:join_strings(", ", RPS) ++ ")";
                ?CONSTRAINTS_BOTTOM ->
                    "#BOTTOM#";
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%===========================================================================================
%% Utility Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list separator for a list of values.
%% @param Values The list to check.
%% @returns The string separator value.
%% @end
%%-------------------------------------------------------------------------------------------
get_list_separator(Values) when is_list(Values) ->
    case type:is_antype(Values) of
        true -> ";\n";
        false -> ", "
    end;
get_list_separator(_) ->
    ", ".

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Converts a type variable name into a string.
%% @param Name The type variable name to convert.
%% @returns The type variable name turn into a string.
%% @end
%%-------------------------------------------------------------------------------------------
name({Module, Name}) ->
    util:to_string(Module) ++ ":" ++ util:to_string(Name);
name(Name) when is_atom(Name) ->
    util:to_string(Name);
name(Name) ->
    "$" ++ util:to_string(Name).
