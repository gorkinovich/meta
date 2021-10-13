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
-module(mono).
-author("Gorka Suárez García").
-export([
    execute/1, is_bottom/1, equals/2, subset/2,
    subseteq/2, infimum/1, infimum/2, supremum/2
]).
-include("type.hrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Executes a constraint to obtain the result.
%% @param Constraints The constraints to execute.
%% @returns The result of executing the constraint.
%% @end
%%-------------------------------------------------------------------------------------------
execute({?CONSTRAINT_MATCH, Left, Right}) ->
    subseteq(Left, Right);
execute({?CONSTRAINT_SUBSETEQ, Left, Right}) ->
    subseteq(Left, Right);
execute(_) ->
    throw(not_supported).

%%===========================================================================================
%% Check Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if some constraints are bottom or not.
%% @param Constraints The constraints to check.
%% @returns 'true' if the constraints are bottom; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_bottom(?CONSTRAINTS_BOTTOM) ->
    true;
is_bottom([]) ->
    false;
is_bottom([Value | Values]) ->
    case execute(Value) of
        true -> is_bottom(Values);
        false -> true
    end;
is_bottom(_) ->
    throw(not_supported).

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
subseteq({?TYPE_VARIABLE, _, _}, _) ->
    throw(not_supported);
subseteq(_, {?TYPE_VARIABLE, _, _}) ->
    throw(not_supported);
subseteq({?TYPE_SCHEME, _, _}, _) ->
    throw(not_supported);
subseteq(_, {?TYPE_SCHEME, _, _}) ->
    throw(not_supported);
subseteq({?TYPE_CONDITION, LT, LC}, Right) ->
    case is_bottom(LC) of
        true -> true;
        false -> subseteq(LT, Right)
    end;
subseteq(Left, {?TYPE_CONDITION, RT, RC}) ->
    case is_bottom(RC) of
        true -> false;
        false -> subseteq(Left, RT)
    end;
subseteq({?TYPE_SET, ?TYPE_NONE, _}, _) ->
    true;
subseteq(_, {?TYPE_SET, ?TYPE_ANY, _}) ->
    true;
subseteq(_, {?TYPE_UNION, _, []}) ->
    false;
subseteq({?TYPE_LITERAL, Left}, {?TYPE_LITERAL, Right}) ->
    Left =:= Right;
subseteq({?TYPE_LITERAL, {}}, {?TYPE_SET, ?TYPE_TUPLE, []}) ->
    true;
subseteq({?TYPE_LITERAL, Left}, {?TYPE_SET, ?TYPE_NELIST, [Body, Tail]}) when is_list(Left) ->
    (Body =:= type:integer() orelse Body =:= type:natural() orelse Body =:= type:char())
        andalso (Tail =:= type:nil());
subseteq({?TYPE_LITERAL, Left}, {?TYPE_SET, ?TYPE_ATOM, []}) when is_atom(Left) ->
    true;
subseteq({?TYPE_LITERAL, Left}, {?TYPE_SET, Name, []}) when is_integer(Left) ->
    case Name of
        ?TYPE_INTEGER -> true;
        ?TYPE_NEGINT  -> Left < 0;
        ?TYPE_NAT     -> Left >= 0;
        ?TYPE_NZNAT   -> Left > 0;
        ?TYPE_ARITY   -> 0 =< Left andalso Left =< 255;
        ?TYPE_BYTE    -> 0 =< Left andalso Left =< 255;
        ?TYPE_CHAR    -> 0 =< Left andalso Left =< 16#10FFFF;
        _             -> false
    end;
subseteq({?TYPE_LITERAL, Left}, {?TYPE_SET, ?TYPE_FLOAT, []}) when is_float(Left) ->
    true;
subseteq({?TYPE_SET, ?TYPE_NEGINT, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NEGINT;
subseteq({?TYPE_SET, ?TYPE_NAT, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NAT;
subseteq({?TYPE_SET, ?TYPE_NZNAT, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NAT orelse
    Name =:= ?TYPE_NZNAT;
subseteq({?TYPE_SET, ?TYPE_ARITY, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NAT orelse
    Name =:= ?TYPE_ARITY orelse
    Name =:= ?TYPE_BYTE orelse
    Name =:= ?TYPE_CHAR;
subseteq({?TYPE_SET, ?TYPE_BYTE, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NAT orelse
    Name =:= ?TYPE_ARITY orelse
    Name =:= ?TYPE_BYTE orelse
    Name =:= ?TYPE_CHAR;
subseteq({?TYPE_SET, ?TYPE_CHAR, []}, {?TYPE_SET, Name, []}) ->
    Name =:= ?TYPE_INTEGER orelse
    Name =:= ?TYPE_NAT orelse
    Name =:= ?TYPE_CHAR;
subseteq({?TYPE_SET, ?TYPE_TUPLE, _}, {?TYPE_SET, ?TYPE_TUPLE, []}) ->
    true;
subseteq({?TYPE_SET, ?TYPE_NELIST, [LeftBody, LeftTail]}, {?TYPE_SET, ?TYPE_NELIST, [RightBody, RightTail]}) ->
    subseteq(LeftBody, RightBody) andalso
        (
            subseteq(LeftTail, RightTail) orelse
            subseteq(LeftTail, {?TYPE_SET, ?TYPE_NELIST, [RightBody, RightTail]})
        );
subseteq({?TYPE_SET, Name, Left}, {?TYPE_SET, Name, Right}) ->
    subseteq_list(Left, Right);
subseteq({?TYPE_LAMBDA, _, _}, {?TYPE_SET, ?TYPE_FUN, []}) ->
    true;
subseteq({?TYPE_LAMBDA, LP, LR}, {?TYPE_LAMBDA, RP, RR}) ->
    subseteq_list(LP, RP) andalso subseteq(LR, RR);
subseteq({?TYPE_UNION, _, Types}, Right) ->
    subseteq_left_list(Types, Right);
subseteq(Left, {?TYPE_UNION, _, Types}) ->
    subseteq_right_list(Left, Types);
subseteq(_, _) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a list of types is subset or equal to another.
%% @param Left The left list of types to compare.
%% @param Right The right list of types to compare.
%% @returns 'true' if the list is subset or equal to the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq_list([], []) ->
    true;
subseteq_list([Left | LeftTypes], [Right | RightTypes]) ->
    case subseteq(Left, Right) of
        true -> subseteq_list(LeftTypes, RightTypes);
        false -> false
    end;
subseteq_list(_, _) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if some types are subset or equal to another.
%% @param Left The left list of types to compare.
%% @param Right The right type to compare.
%% @returns 'true' if the types are subset or equal to the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq_left_list([], _) ->
    true;
subseteq_left_list([Type | Types], Right) ->
    case subseteq(Type, Right) of
        true -> subseteq_left_list(Types, Right);
        false -> false
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if some types are subset or equal to another.
%% @param Left The left type to compare.
%% @param Right The right list of types to compare.
%% @returns 'true' if the types are subset or equal to the other; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
subseteq_right_list(_, []) ->
    false;
subseteq_right_list(Left, [Type | Types]) ->
    case subseteq(Left, Type) of
        true -> true;
        false -> subseteq_right_list(Left, Types)
    end.

%%===========================================================================================
%% Operation Functions
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
%% @returns The new type obtained from the operation.
%% @end
%%-------------------------------------------------------------------------------------------
infimum(Left, Right) ->
    normalize:recursive(general_infimum(Left, Right)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
% Not supported types section:
general_infimum({?TYPE_VARIABLE, _, _}, _) ->
    throw(not_supported);
general_infimum(_, {?TYPE_VARIABLE, _, _}) ->
    throw(not_supported);
general_infimum({?TYPE_SCHEME, _, _}, _) ->
    throw(not_supported);
general_infimum(_, {?TYPE_SCHEME, _, _}) ->
    throw(not_supported);
% Same types section:
general_infimum(Type, Type) ->
    Type;
% Conditional types section:
general_infimum({?TYPE_CONDITION, LT, LC}, Right) ->
    case is_bottom(LC) of
        true -> type:none();
        false -> general_infimum(LT, Right)
    end;
general_infimum(Left, {?TYPE_CONDITION, RT, RC}) ->
    case is_bottom(RC) of
        true -> type:none();
        false -> general_infimum(Left, RT)
    end;
% 'none()' and 'any()' section:
general_infimum({?TYPE_SET, ?TYPE_NONE, _}, _) ->
    type:none();
general_infimum(_, {?TYPE_SET, ?TYPE_NONE, _}) ->
    type:none();
general_infimum({?TYPE_SET, ?TYPE_ANY, _}, Right) ->
    Right;
general_infimum(Left, {?TYPE_SET, ?TYPE_ANY, _}) ->
    Left;
% Union types section:
general_infimum({?TYPE_UNION, LeftOrdered, Left}, {?TYPE_UNION, RightOrdered, Right}) ->
    Types = [general_infimum(L, R) || L <- Left, R <- Right],
    make_union(LeftOrdered andalso RightOrdered, Types);
general_infimum({?TYPE_UNION, LeftOrdered, Left}, Right) ->
    Types = [general_infimum(L, Right) || L <- Left],
    make_union(LeftOrdered, Types);
general_infimum(Left, {?TYPE_UNION, RightOrdered, Right}) ->
    Types = [general_infimum(Left, R) || R <- Right],
    make_union(RightOrdered, Types);
% Nelist types section:
general_infimum({?TYPE_SET, ?TYPE_NELIST, [LeftBody, LeftTail]}, {?TYPE_SET, ?TYPE_NELIST, [RightBody, RightTail]}) ->
    type:nelist(
        general_infimum(LeftBody, RightBody),
        type:union([
            general_infimum(LeftTail, RightTail),
            general_infimum({?TYPE_SET, ?TYPE_NELIST, [LeftBody, LeftTail]}, RightTail),
            general_infimum(LeftTail, {?TYPE_SET, ?TYPE_NELIST, [RightBody, RightTail]})
        ])
    );
% Set types section:
general_infimum({?TYPE_SET, ?TYPE_TUPLE, Types}, {?TYPE_SET, ?TYPE_TUPLE, []}) ->
    {?TYPE_SET, ?TYPE_TUPLE, Types};
general_infimum({?TYPE_SET, ?TYPE_TUPLE, []}, {?TYPE_SET, ?TYPE_TUPLE, Types}) ->
    {?TYPE_SET, ?TYPE_TUPLE, Types};
general_infimum({?TYPE_SET, Name, Left}, {?TYPE_SET, Name, Right}) ->
    case length(Left) =/= length(Right) of
        true ->
            type:none();
        _ ->
            type:set(Name, [general_infimum(L, R) || {L, R} <- lists:zip(Left, Right)])
    end;
% Lambda types section:
general_infimum(Left = {?TYPE_LAMBDA, _, _}, {?TYPE_SET, ?TYPE_FUN, []}) ->
    Left;
general_infimum({?TYPE_SET, ?TYPE_FUN, []}, Right = {?TYPE_LAMBDA, _, _}) ->
    Right;
general_infimum({?TYPE_LAMBDA, LeftParams, LeftResult}, {?TYPE_LAMBDA, RightParams, RightResult}) ->
    case length(LeftParams) =/= length(RightParams) of
        true ->
             type:none();
        _ ->
            type:lambda(
                [general_infimum(L, R) || {L, R} <- lists:zip(LeftParams, RightParams)],
                general_infimum(LeftResult, RightResult)
            )
    end;
% Literal types section:
general_infimum({?TYPE_LITERAL, Value}, Right) ->
    literal_infimum({?TYPE_LITERAL, Value}, Right);
general_infimum(Left, {?TYPE_LITERAL, Value}) ->
    literal_infimum({?TYPE_LITERAL, Value}, Left);
general_infimum(Left = {?TYPE_SET, _, []}, Right = {?TYPE_SET, _, []}) ->
    sets_infimum(Left, Right);
% Default section:
general_infimum(_, _) ->
    type:none().

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
literal_infimum(Literal, Type) ->
    case subseteq(Literal, Type) of
        true -> Literal;
        _ -> type:none()
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
sets_infimum(Left, Right) ->
    case subseteq(Left, Right) of
        true -> Left;
        false ->
            case subseteq(Right, Left) of
                true -> Right;
                false -> type:none()
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun general_infimum/2'.
%% @end
%%-------------------------------------------------------------------------------------------
make_union(Ordered, Types) ->
    case Ordered of
        true -> type:sequence(Types);
        _ -> type:union(Types)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the supremum between two types.
%% @param Left The left type of the operation.
%% @param Right The right type of the operation.
%% @returns The new type obtained from the operation.
%% @end
%%-------------------------------------------------------------------------------------------
supremum(Left, Right) ->
    case subseteq(Left, Right) of
        true -> Right;
        false ->
            case subseteq(Right, Left) of
                true -> Left;
                false -> type:union(Left, Right)
            end
    end.
