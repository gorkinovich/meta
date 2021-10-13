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
-module(type).
-author("Gorka Suárez García").
-export([
    % Make General Types:
    pair/1, pair/2, variable/1, variable/2, literal/1, set/1, set/2,
    scheme/2, lambda/2, condition/2, union/1, union/2, sequence/1,
    sequence/2, union_or_sequence/2,

    % Make Symbol Types:
    symbol_call/2,

    % Make Basic Types:
    any/0, none/0, atom/0, integer/0, float/0, tuple/0,
    tuple/1, nelist/0, nelist/1, nelist/2,

    % Make Extended Types:
    function/0, map/0, binary/0, pid/0, port/0, reference/0, neginteger/0,
    natural/0, nznatural/0, arity/0, byte/0, char/0,

    % Make Basic Alias:
    noreturn/0, boolean/0, number/0, nil/0, list/0, list/1, maybe_improper_list/0,
    string/0, nestring/0, timeout/0, node/0, module/0, mfa/0, identifier/0,

    % Lambda Functions:
    lambda_any/1, lambda_none/1,

    % Merge Functions:
    merge/1, merge/2, mergex/2, mergex/3,

    % Query Functions:
    is_antype/1, is_type/1, is_pair/1, is_variable/1, is_literal/1, is_set/1,
    is_scheme/1, is_lambda/1, is_condition/1, is_union/1, is_sequence/1,
    is_symbol/1, is_any/1, is_none/1, is_lambda_any/1, is_lambda_none/1,
    is_abstraction/1, is_monomorphic/1, is_bottom/1, contains_none/1,
    has_conditional/1,

    % Properties Functions:
    get_variable_name/1, get_literal_value/1, get_inner_type/1,

    % Transform Functions:
    to_nelist/1, clear_variables/1, rename/2, rename_free_variables/2,
    substitute/3, substitute_variable/3, substitute_variables/2,
    substitute_variables/3, set_linked_flag/3, remove_symbol_constraints/1,
    map/2, filter/2, map_wfvs/2, map_wfvs/4
]).
-include("type.hrl").
-include("language.hrl").

%%===========================================================================================
%% Make Functions (General Types)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a pair type.
%% @param Type The type related to the constraints.
%% @returns The new annotated type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
pair(Type) -> {?TYPE_PAIR, Type, constraint:conjunction()}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a pair type.
%% @param Type The type related to the constraints.
%% @param Constraints The set of constraints of the type.
%% @returns The new annotated type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
pair(Type, Constraints) -> {?TYPE_PAIR, Type, Constraints}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a type variable.
%% @param Name The type variable name.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
variable(Name) ->
    variable(Name, false).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a type variable.
%% @param Name The type variable name.
%% @param Linked The is linked to an open program variable flag.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
variable(Name, Linked) when is_list(Name) ->
    {?TYPE_VARIABLE, list_to_atom(Name), Linked};
variable(Name, Linked) ->
    {?TYPE_VARIABLE, Name, Linked}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a literal type.
%% @param Value The literal value.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
literal(Value) ->
    {?TYPE_LITERAL, Value}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a set type.
%% @param Name The set name.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
set(Name) ->
    set(Name, []).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a set type.
%% @param Name The set name.
%% @param Parameters The parameters of the set.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
set(Name, Parameters) when is_atom(Name), is_list(Parameters) ->
    {?TYPE_SET, Name, Parameters}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a scheme type.
%% @param Variables The set of closed type variables.
%% @param Inner The inner type of the scheme.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
scheme([], Inner) when is_tuple(Inner) ->
    Inner;
scheme(Variables, Inner) when is_list(Variables), is_tuple(Inner) ->
    {?TYPE_SCHEME, Variables, Inner}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a lambda type.
%% @param Parameters The types of the parameters.
%% @param Result The type of the result.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
lambda(Parameters, Result) when is_list(Parameters), is_tuple(Result) ->
    {?TYPE_LAMBDA, Parameters, Result}.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a conditional type.
%% @param Type The type related to the constraints.
%% @param Constraints The set of constraints of the type.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
condition(_, ?CONSTRAINTS_BOTTOM) ->
    none();
condition(Type, Constraints) when is_tuple(Type), is_list(Constraints) ->
    case is_type(Type) of
        false ->
            none();
        true ->
            case Constraints of
                [] -> Type;
                _ -> {?TYPE_CONDITION, Type, Constraints}
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a union type.
%% @param Types The list of types in the union.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
union(Types) when is_list(Types) -> make_union(Types);
union({?TYPE_UNION, false, Types}) -> make_union(Types);
union(Type) -> make_union([Type]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a union type.
%% @param LeftType The left type in the union.
%% @param RightType The right type in the union.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
union(LeftType, RightType) ->
    make_union(util:list_append(LeftType, RightType)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a sequence type.
%% @param Types The list of types in the sequence.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
sequence(Types) when is_list(Types) -> make_sequence(Types);
sequence({?TYPE_UNION, true, Types}) -> make_sequence(Types);
sequence(Type) -> make_sequence([Type]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a sequence type.
%% @param LeftType The left type in the sequence.
%% @param RightType The right type in the sequence.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
sequence(LeftType, RightType) ->
    make_sequence(util:list_append(LeftType, RightType)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a union or sequence type.
%% @param Types The list of types in the union or sequence.
%% @param Ordered The union or sequence flag.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
union_or_sequence(true, Type) -> sequence(Type);
union_or_sequence(_, Type) -> union(Type).

%%===========================================================================================
%% Make Functions (Symbol Types)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of a symbolic call type.
%% @param Variable The called function.
%% @param Parameters The parameters of the call.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
symbol_call(Variable, Parameters) ->
    {?TYPE_SYMBOL_CALL, Variable, Parameters}.

%%===========================================================================================
%% Make Functions (Basic Types)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'any()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
any() -> set(?TYPE_ANY).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'none()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
none() -> set(?TYPE_NONE).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'atom()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
atom() -> set(?TYPE_ATOM).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'integer()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
integer() -> set(?TYPE_INTEGER).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'float()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
float() -> set(?TYPE_FLOAT).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'tuple()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
tuple() -> set(?TYPE_TUPLE).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type '{F1,...,FN}'.
%% @param Fields The types of the fields.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
tuple(Fields) -> set(?TYPE_TUPLE, Fields).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nelist(any(),nil())' (non-empty list).
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nelist() -> set(?TYPE_NELIST, [any(), nil()]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nelist(B,nil())' (non-empty list).
%% @param Body The type of the body.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nelist(Body) -> set(?TYPE_NELIST, [Body, nil()]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nelist(B,T)' (non-empty list).
%% @param Body The type of the body.
%% @param Tail The type of the tail.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nelist(Body, Tail) -> set(?TYPE_NELIST, [Body, Tail]).

%%===========================================================================================
%% Make Functions (Extended Types)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'fun()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
function() -> set(?TYPE_FUN).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'map()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
map() -> set(?TYPE_MAP).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'binary()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
binary() -> set(?TYPE_BIN).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'pid()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
pid() -> set(?TYPE_PID).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'port()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
port() -> set(?TYPE_PORT).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'reference()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
reference() -> set(?TYPE_REF).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'negint()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
neginteger() -> set(?TYPE_NEGINT).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'natural()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
natural() -> set(?TYPE_NAT).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nznat()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nznatural() -> set(?TYPE_NZNAT).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'arity()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
arity() -> set(?TYPE_ARITY).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'byte()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
byte() -> set(?TYPE_BYTE).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'char()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
char() -> set(?TYPE_CHAR).

%%===========================================================================================
%% Make Functions (Basic Alias)
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'noreturn()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
noreturn() -> none().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'boolean()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
boolean() -> union([literal(?TRUE), literal(?FALSE)]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'number()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
number() -> union([integer(), float()]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nil()' or '[]'.
%% This type is a subset of any list type.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nil() -> literal([]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'list()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
list() -> union([literal([]), nelist()]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'list(Type)'.
%% @param Type The type of the list.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
list(Type) -> union([literal([]), nelist(Type)]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'maybe_improper_list()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
maybe_improper_list() -> union([literal([]), nelist(any(), any())]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'string()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
string() -> union([literal([]), nelist(char())]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'nestring()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
nestring() -> union([literal([]), nelist(char())]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'timeout()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
timeout() -> union([integer(), literal(?INFINITY)]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'node()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
node() -> atom().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'module()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
module() -> atom().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'mfa()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
mfa() -> tuple([module(), atom(), arity()]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of the type 'identifier()'.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
identifier() -> union([pid(), port(), reference()]).

%%===========================================================================================
%% Union and Sequence Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes an instance of a union type.
%% @param Victims The list of types in the union.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
make_union(Victims) when is_list(Victims) ->
    Types = flatten(?TYPE_UNION, false, Victims, []),
    make(?TYPE_UNION, false, Types).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes an instance of a sequence type.
%% @param Victims The list of types in the sequence.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
make_sequence(Victims) when is_list(Victims) ->
    Types = flatten(?TYPE_UNION, true, Victims, []),
    make(?TYPE_UNION, true, Types).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes an instance of a type.
%% @param Class The class of the type.
%% @param Ordered The ordered flag of the type.
%% @param Values The list of types in the type.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
make(_, _, []) -> none();
make(_, _, [Value]) -> Value;
make(Class, Ordered, Values) when is_list(Values) -> {Class, Ordered, Values}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Flattens the elements of a list.
%% @param Class The class of the type.
%% @param Ordered The ordered flag of the type.
%% @param Values The list of types to flatten.
%% @param Types The accumulated list of types.
%% @returns The new flatten list.
%% @end
%%-------------------------------------------------------------------------------------------
flatten(?TYPE_UNION, true, [], Types) ->
    remove_duplicates(Types, []);
flatten(?TYPE_UNION, false, [], Types) ->
    lists:usort(Types);
flatten(Class, Ordered, [{Class, Ordered, InnerTypes} | Values], Types) ->
    flatten(Class, Ordered, Values, util:list_append(Types, InnerTypes));
flatten(Class, Ordered, [Value | Values], Types) ->
    case is_none(Value) of
        true -> flatten(Class, Ordered, Values, Types);
        _ -> flatten(Class, Ordered, Values, util:list_append(Types, Value))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Removes the duplicates in a sequence of types.
%% @param Types The list of types of the sequence.
%% @param Result The final list of types of the sequence.
%% @returns The new flatten list.
%% @end
%%-------------------------------------------------------------------------------------------
remove_duplicates([], Result) ->
    lists:reverse(Result);
remove_duplicates([Type | Types], Result) ->
    case lists:member(Type, Result) of
        true -> remove_duplicates(Types, Result);
        _ -> remove_duplicates(Types, [Type | Result])
    end.

%%===========================================================================================
%% Lambda Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of an all 'any()' lambda type.
%% @param Size The number of parameters.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
lambda_any(Size) when is_integer(Size), Size >= 0 ->
    lambda(lists:duplicate(Size, any()), any()).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an instance of an all 'none()' lambda type.
%% @param Size The number of parameters.
%% @returns The new type definition instance.
%% @end
%%-------------------------------------------------------------------------------------------
lambda_none(Size) when is_integer(Size), Size >= 0 ->
    lambda(lists:duplicate(Size, none()), none()).

%%===========================================================================================
%% Merge Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a list of annotated types into one.
%% @param Types The list with the annotated type to merge.
%% @returns The merged annotated type.
%% @end
%%-------------------------------------------------------------------------------------------
merge(Types) ->
    mergex(Types, fun tuple/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a pair of annotated types into one.
%% @param Left The left annotated type to merge.
%% @param Right The right annotated type to merge.
%% @returns The merged annotated type.
%% @end
%%-------------------------------------------------------------------------------------------
merge(Left, Right) ->
    mergex(Left, Right, fun tuple/1).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a list of annotated types into one.
%% @param Types The list with the annotated type to merge.
%% @param Ctor The constructor function to merge the types.
%% @returns The merged annotated type.
%% @end
%%-------------------------------------------------------------------------------------------
mergex([], Ctor) ->
    [pair(Ctor([]))];
mergex([Victim], Ctor) when is_list(Victim) ->
    [pair(Ctor([T]), CS) || {_, T, CS} <- Victim];
mergex([{?TYPE_PAIR, Type, Constraints}], Ctor) ->
    [pair(Ctor([Type]), Constraints)];
mergex(Types, Ctor) when is_list(Types) ->
    Pairs = util:list_product(Types),
    [merge_pairs(Item, Ctor) || Item <- Pairs].

%%-------------------------------------------------------------------------------------------
%% @doc
%% Merges a pair of annotated types into one.
%% @param Left The left annotated type to merge.
%% @param Right The right annotated type to merge.
%% @param Ctor The constructor function to merge the types.
%% @returns The merged annotated type.
%% @end
%%-------------------------------------------------------------------------------------------
mergex(Left, Right, Ctor) when is_tuple(Left), is_tuple(Right) ->
    merge_pairs([Left, Right], Ctor);
mergex(Left, Right, Ctor) when is_list(Left), is_tuple(Right) ->
    [merge_pairs([L, Right], Ctor) || L <- Left];
mergex(Left, Right, Ctor) when is_tuple(Left), is_list(Right) ->
    [merge_pairs([Left, R], Ctor) || R <- Right];
mergex(Left, Right, Ctor) when is_list(Left), is_list(Right) ->
    mergex([Left, Right], Ctor).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Merges a list of pair types into one.
%% @param Pairs The list with the pair types to merge.
%% @param Ctor The constructor function to merge the types.
%% @returns The merged annotated type.
%% @end
%%-------------------------------------------------------------------------------------------
merge_pairs(Pairs, Ctor) when is_list(Pairs) ->
    {Types, Constraints} = lists:unzip([{TP, CS} || {_, TP, CS} <- Pairs]),
    pair(Ctor(Types), lists:flatten(Constraints)).

%%===========================================================================================
%% Query Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is an annotated type.
%% @param Value The value to check.
%% @returns 'true' if the value is an annotated type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_antype(Values) when is_list(Values) ->
    util:flags_all([is_pair(V) || V <- Values]);
is_antype({?TYPE_PAIR, _, _}) -> true;
is_antype(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type.
%% @param Value The value to check.
%% @returns 'true' if the value is a type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_type(Values) when is_list(Values) ->
    util:flags_all([is_type(V) || V <- Values]);
is_type({?TYPE_VARIABLE, _, _}) -> true;
is_type({?TYPE_LITERAL, _}) -> true;
is_type({?TYPE_SET, _, _}) -> true;
is_type({?TYPE_SCHEME, _, _}) -> true;
is_type({?TYPE_LAMBDA, _, _}) -> true;
is_type({?TYPE_CONDITION, _, _}) -> true;
is_type({?TYPE_UNION, _, _}) -> true;
is_type({?TYPE_SYMBOL_CALL, _, _}) -> true;
is_type(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a pair type.
%% @param Value The value to check.
%% @returns 'true' if the value is a pair type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_pair({?TYPE_PAIR, _, _}) -> true;
is_pair(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type variable.
%% @param Value The value to check.
%% @returns 'true' if the value is a type variable; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_variable({?TYPE_VARIABLE, _, _}) -> true;
is_variable(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type literal.
%% @param Value The value to check.
%% @returns 'true' if the value is a type literal; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_literal({?TYPE_LITERAL, _}) -> true;
is_literal(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type set.
%% @param Value The value to check.
%% @returns 'true' if the value is a type set; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_set({?TYPE_SET, _, _}) -> true;
is_set(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type scheme.
%% @param Value The value to check.
%% @returns 'true' if the value is a type scheme; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_scheme({?TYPE_SCHEME, _, _}) -> true;
is_scheme(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type lambda.
%% @param Value The value to check.
%% @returns 'true' if the value is a type lambda; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_lambda({?TYPE_LAMBDA, _, _}) -> true;
is_lambda(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a conditional type.
%% @param Value The value to check.
%% @returns 'true' if the value is a conditional type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_condition({?TYPE_CONDITION, _, _}) -> true;
is_condition(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type union.
%% @param Value The value to check.
%% @returns 'true' if the value is a type union; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_union({?TYPE_UNION, false, _}) -> true;
is_union(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type sequence.
%% @param Value The value to check.
%% @returns 'true' if the value is a type sequence; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_sequence({?TYPE_UNION, true, _}) -> true;
is_sequence(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type symbol.
%% @param Value The value to check.
%% @returns 'true' if the value is a type symbol; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_symbol({?TYPE_SYMBOL_CALL, _, _}) -> true;
is_symbol(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is the type 'any()'.
%% @param Value The value to check.
%% @returns 'true' if the value is 'any()'; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_any({?TYPE_SET, ?TYPE_ANY, _}) -> true;
is_any(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is the type 'none()'.
%% @param Value The value to check.
%% @returns 'true' if the value is 'none()'; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_none({?TYPE_SET, ?TYPE_NONE, _}) -> true;
is_none(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is the 'any()' lambda type.
%% @param Value The value to check.
%% @returns 'true' if the value is the 'any()' lambda type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_lambda_any({?TYPE_LAMBDA, Parameters, Result}) ->
    is_any(Result) andalso (not lists:member(false, [is_any(Victim) || Victim <- Parameters]));
is_lambda_any({?TYPE_SCHEME, _, InnerType}) ->
    is_lambda_any(InnerType);
is_lambda_any({?TYPE_UNION, _, Types}) ->
    lists:member(true, [is_lambda_any(Type) || Type <- Types]);
is_lambda_any(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is the 'none()' lambda type.
%% @param Value The value to check.
%% @returns 'true' if the value is the 'none()' lambda type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_lambda_none({?TYPE_LAMBDA, Parameters, Result}) ->
    is_none(Result) orelse lists:member(true, [is_none(Victim) || Victim <- Parameters]);
is_lambda_none({?TYPE_SCHEME, _, InnerType}) ->
    is_lambda_none(InnerType);
is_lambda_none({?TYPE_UNION, _, Types}) ->
    not lists:member(false, [is_lambda_none(Type) || Type <- Types]);
is_lambda_none(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a type abstraction. The kind of abstractions are lambdas,
%% scheme lambdas, and sequence or union of abstractions.
%% @param Value The value to check.
%% @returns 'true' if the value is a type abstraction; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_abstraction({?TYPE_SCHEME, _, {?TYPE_LAMBDA, _, _}}) ->
    true;
is_abstraction({?TYPE_LAMBDA, _, _}) ->
    true;
is_abstraction({?TYPE_UNION, _, Types}) ->
    util:flags_all([is_abstraction(T) || T <- Types]);
is_abstraction(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a value is a monomorphic type.
%% @param Value The value to check.
%% @returns 'true' if the value is a monomorphic type; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_monomorphic({?TYPE_SYMBOL_CALL, _, _}) ->
    false;
is_monomorphic(Type) ->
    query:count_all_variables(Type) =< 0.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if the a type is bottom.
%% @param Value The value to analyze.
%% @returns Returns 'true' when is bottom.
%% @end
%%-------------------------------------------------------------------------------------------
is_bottom(Values) when is_list(Values) ->
    not lists:member(false, [is_bottom(Value) || Value <- Values]);
is_bottom({?TYPE_PAIR, Type, Constraints}) ->
    is_none(Type) orelse Constraints =:= ?CONSTRAINTS_BOTTOM;
is_bottom(_) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a list of types contains the type 'none()'.
%% @param Value The list value to check.
%% @returns Returns 'true' when the list contains 'none()'.
%% @end
%%-------------------------------------------------------------------------------------------
contains_none([]) ->
    false;
contains_none([Value | Values]) ->
    case is_none(Value) of
        true -> true;
        false -> contains_none(Values)
    end;
contains_none(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a type has a conditional type inside.
%% @param Value The type value to analyze.
%% @returns 'true' when the type has a conditional type inside.
%% @end
%%-------------------------------------------------------------------------------------------
has_conditional(Value) ->
    map(Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_PAIR, RT, RCS} -> RT orelse util:flags_any(RCS);
                {?TYPE_SET, _, RPS} -> util:flags_any(RPS);
                {?TYPE_SCHEME, _, RI} -> RI;
                {?TYPE_LAMBDA, RPS, RR} -> util:flags_any(RPS) orelse RR;
                {?TYPE_CONDITION, _, _} -> true;
                {?TYPE_UNION, _, RTS} -> util:flags_any(RTS);
                {?CONSTRAINT_MATCH, RL, RR} -> RL orelse RR;
                {?CONSTRAINT_SUBSETEQ, RL, RR} -> RL orelse RR;
                _ -> false
            end
        end
    ).

%%===========================================================================================
%% Properties Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the name of a type variable.
%% @param Value The value to check.
%% @returns The name of the type variable.
%% @end
%%-------------------------------------------------------------------------------------------
get_variable_name({?TYPE_VARIABLE, Name, _}) -> Name;
get_variable_name(Name) -> Name.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the value of a literal type.
%% @param Value The value to check.
%% @returns The value of the literal variable.
%% @end
%%-------------------------------------------------------------------------------------------
get_literal_value({?TYPE_LITERAL, Value}) -> Value;
get_literal_value(_) -> throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the inner type inside a scheme.
%% @param Value The value to check.
%% @returns The inner type inside the scheme.
%% @end
%%-------------------------------------------------------------------------------------------
get_inner_type({?TYPE_SCHEME, _, Value}) -> Value;
get_inner_type(Value) -> Value.

%%===========================================================================================
%% Transform Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Converts a tuple of two fields into a non-empty list type.
%% @param Value The value to convert.
%% @returns The value turn into a non-empty list.
%% @end
%%-------------------------------------------------------------------------------------------
to_nelist({?TYPE_SET, ?TYPE_TUPLE, [Head, Tail]}) ->
    nelist(Head, Tail);
to_nelist(_) ->
    none().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Clears the unused variables inside a scheme type.
%% @param Value The scheme type to change.
%% @returns The scheme type changed.
%% @end
%%-------------------------------------------------------------------------------------------
clear_variables({?TYPE_SCHEME, Variables, Inner}) ->
    Names = query:get_free_variables(Inner),
    NV = util:list_intersection(Variables, Names),
    scheme(NV, Inner);
clear_variables(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Renames a collection of type variables.
%% @param Root The root value to change.
%% @param Names The map of new names.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
rename(Root, Names) when is_map(Names) ->
    map(Root,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, Linked} ->
                    {?TYPE_VARIABLE, util:map_get(Names, Name, Name), Linked};
                {?TYPE_SCHEME, RVS, RI} ->
                    {?TYPE_SCHEME, [util:map_get(Names, Item, Item) || Item <- RVS], RI};
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Renames a collection of free type variables.
%% @param Root The root value to change.
%% @param Names The map of new names.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
rename_free_variables(Value, Names) ->
    case Value of
        Values when is_list(Values) ->
            [rename_free_variables(Item, Names) || Item <- Values];
        {?TYPE_PAIR, Type, Constraints} ->
            {?TYPE_PAIR, rename_free_variables(Type, Names), rename_free_variables(Constraints, Names)};
        {?TYPE_VARIABLE, Name, Linked} ->
            {?TYPE_VARIABLE, util:substitution(Name, Names), Linked};
        {?TYPE_SET, Name, Parameters} ->
            {?TYPE_SET, Name, rename_free_variables(Parameters, Names)};
        {?TYPE_SCHEME, Variables, Inner} ->
            case Names of
                % Names is a tuple with an old name and a new name:
                {OldName, _} ->
                    case lists:member(OldName, Variables) of
                        true ->
                            {?TYPE_SCHEME, Variables, Inner};
                        _ ->
                            {?TYPE_SCHEME, Variables, rename_free_variables(Inner, Names)}
                    end;
                % Names is a map table with old names and new names:
                _ ->
                    InnerNames = util:map_remove_keys(Names, Variables),
                    {?TYPE_SCHEME, Variables, rename_free_variables(Inner, InnerNames)}
            end;
        {?TYPE_LAMBDA, Parameters, Result} ->
            {?TYPE_LAMBDA, rename_free_variables(Parameters, Names), rename_free_variables(Result, Names)};
        {?TYPE_CONDITION, Type, Constraints} ->
            {?TYPE_CONDITION, rename_free_variables(Type, Names), rename_free_variables(Constraints, Names)};
        {?TYPE_UNION, Ordered, Types} ->
            {?TYPE_UNION, Ordered, rename_free_variables(Types, Names)};
        {?CONSTRAINT_MATCH, Variable, Type} ->
            {?CONSTRAINT_MATCH, rename_free_variables(Variable, Names), rename_free_variables(Type, Names)};
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            {?CONSTRAINT_SUBSETEQ, rename_free_variables(LeftType, Names), rename_free_variables(RightType, Names)};
        {?CONSTRAINT_JOINABLE, Variables} ->
            {?CONSTRAINT_JOINABLE, rename_free_variables(Variables, Names)};
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            {?TYPE_SYMBOL_CALL, rename_free_variables(Variable, Names), rename_free_variables(Parameters, Names)};
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Substitutes an inner value with a new one.
%% @param Root The root value to change.
%% @param FindValue The value to find inside.
%% @param NextValue The new value to return.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
substitute(Root, FindValue, NextValue) ->
    map(Root,
        fun(Victim) ->
            case Victim of
                FindValue -> NextValue;
                Otherwise -> Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Substitutes an inner value with a new one.
%% @param Root The root value to change.
%% @param Name The name of the variable.
%% @param Value The new value to return.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
substitute_variable(Root, Name, Value) ->
    map(Root,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, _} -> Value;
                Otherwise -> Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Substitutes an inner value with a new one.
%% @param Root The root value to change.
%% @param Table The table of substitutions.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
substitute_variables(Root, Table) when is_map(Table) ->
    map(Root,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, Linked} ->
                    util:map_get(Table, Name, {?TYPE_VARIABLE, Name, Linked});
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Substitutes an inner value with a new one.
%% @param Root The root value to change.
%% @param Table The table of substitutions.
%% @param Default The default value.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
substitute_variables(Root, Table, Default) when is_map(Table) ->
    map(Root,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, _} ->
                    util:map_get(Table, Name, Default);
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Changes the linked flag of a variable type.
%% @param Value The value to change.
%% @param Name The name of the variable.
%% @param Linked The new value for the flag.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
set_linked_flag(Value, [], _) ->
    Value;
set_linked_flag(Value, [Name | Names], Linked) ->
    set_linked_flag(set_linked_flag(Value, Name, Linked), Names, Linked);
set_linked_flag(Value, Name, Linked) ->
    map(Value,
        fun(Victim) ->
            case Victim of
                {?TYPE_VARIABLE, Name, _} ->
                    {?TYPE_VARIABLE, Name, Linked};
                Otherwise ->
                    Otherwise
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes constraints with symbol types.
%% @param Value The type value to change.
%% @returns The value changed.
%% @end
%%-------------------------------------------------------------------------------------------
remove_symbol_constraints(Value) ->
    filter(Value,
        fun(Victim) ->
            case Victim of
                {?CONSTRAINT_MATCH, _, Right} ->
                    not is_symbol(Right);
                {?CONSTRAINT_SUBSETEQ, _, Right} ->
                    not is_symbol(Right);
                _ ->
                    true
            end
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps a function over a type structure.
%% @param Value The value to map.
%% @param Func The function to apply.
%% @returns The new mapped value.
%% @end
%%-------------------------------------------------------------------------------------------
map(Value, Func) ->
    case Value of
        Values when is_list(Values) ->
            [map(Item, Func) || Item <- Values];
        {?TYPE_PAIR, Type, Constraints} ->
            Func({?TYPE_PAIR, map(Type, Func), map(Constraints, Func)});
        {?TYPE_VARIABLE, Name, Linked} ->
            Func({?TYPE_VARIABLE, Name, Linked});
        {?TYPE_LITERAL, Literal} ->
            Func({?TYPE_LITERAL, Literal});
        {?TYPE_SET, Name, Parameters} ->
            Func({?TYPE_SET, Name, map(Parameters, Func)});
        {?TYPE_SCHEME, Variables, Inner} ->
            Func({?TYPE_SCHEME, map(Variables, Func), map(Inner, Func)});
        {?TYPE_LAMBDA, Parameters, Result} ->
            Func({?TYPE_LAMBDA, map(Parameters, Func), map(Result, Func)});
        {?TYPE_CONDITION, Type, Constraints} ->
            Func({?TYPE_CONDITION, map(Type, Func), map(Constraints, Func)});
        {?TYPE_UNION, Ordered, Types} ->
            Func({?TYPE_UNION, Ordered, map(Types, Func)});
        {?CONSTRAINT_MATCH, Variable, Type} ->
            Func({?CONSTRAINT_MATCH, map(Variable, Func), map(Type, Func)});
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            Func({?CONSTRAINT_SUBSETEQ, map(LeftType, Func), map(RightType, Func)});
        {?CONSTRAINT_JOINABLE, Variables} ->
            Func({?CONSTRAINT_JOINABLE, map(Variables, Func)});
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            Func({?TYPE_SYMBOL_CALL, map(Variable, Func), map(Parameters, Func)});
        _ ->
            Func(Value)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Filters a type structure with a predicate.
%% @param Value The value to filter.
%% @param Pred The predicate to apply.
%% @returns The new filtered value.
%% @end
%%-------------------------------------------------------------------------------------------
filter(Value, Pred) ->
    case Value of
        Victim when is_list(Victim) ->
            [filter(Item, Pred) || Item <- Victim, Pred(Item)];
        {?TYPE_PAIR, Type, Constraints} ->
            pair(filter(Type, Pred), filter(Constraints, Pred));
        {?TYPE_SET, Name, Parameters} ->
            set(Name, filter(Parameters, Pred));
        {?TYPE_SCHEME, Variables, Inner} ->
            scheme(filter(Variables, Pred), filter(Inner, Pred));
        {?TYPE_LAMBDA, Parameters, Result} ->
            lambda(filter(Parameters, Pred), filter(Result, Pred));
        {?TYPE_CONDITION, Type, Constraints} ->
            condition(filter(Type, Pred), filter(Constraints, Pred));
        {?TYPE_UNION, Ordered, Types} ->
            union_or_sequence(Ordered, filter(Types, Pred));
        {?CONSTRAINT_MATCH, Variable, Type} ->
            constraint:match(filter(Variable, Pred), filter(Type, Pred));
        {?CONSTRAINT_SUBSETEQ, Variable, Type} ->
            constraint:subseteq(filter(Variable, Pred), filter(Type, Pred));
        {?CONSTRAINT_JOINABLE, Variables} ->
            constraint:joinable(filter(Variables, Pred));
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            symbol_call(filter(Variable, Pred), filter(Parameters, Pred));
        Otherwise ->
            Otherwise
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps a function over a type structure.
%% @param Value The value to map_wfvs.
%% @param Func The function to apply.
%% @returns The new mapped value.
%% @end
%%-------------------------------------------------------------------------------------------
map_wfvs(Value, Func) ->
    map_wfvs(Value, Func, [], true).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps a function over a type structure.
%% @param Value The value to map_wfvs.
%% @param Func The function to apply.
%% @param FreeVars The external free type variables.
%% @param Flag If true dives into schemes.
%% @returns The new mapped value.
%% @end
%%-------------------------------------------------------------------------------------------
map_wfvs(Value, Func, FreeVars, Flag) ->
    case Value of
        Values when is_list(Values) ->
            map_wfvs_list(Values, [], Func, FreeVars, Flag);
        {?TYPE_PAIR, Type, Constraints} ->
            {NextType, NextConstraints} = map_wfvs_tuple(Type, Constraints, Func, FreeVars, Flag),
            Func({?TYPE_PAIR, NextType, NextConstraints}, FreeVars);
        {?TYPE_VARIABLE, Name, Linked} ->
            Func({?TYPE_VARIABLE, Name, Linked}, FreeVars);
        {?TYPE_LITERAL, Literal} ->
            Func({?TYPE_LITERAL, Literal}, FreeVars);
        {?TYPE_SET, Name, Parameters} ->
            Func({?TYPE_SET, Name, map_wfvs(Parameters, Func, FreeVars, Flag)}, FreeVars);
        {?TYPE_SCHEME, Variables, Inner} ->
            case Flag of
                true ->
                    IFVS = FreeVars -- Variables,
                    NextVariables = map_wfvs(Variables, Func, IFVS, Flag),
                    NextInner = map_wfvs(Inner, Func, IFVS, Flag),
                    Func({?TYPE_SCHEME, NextVariables, NextInner}, FreeVars);
                _ ->
                    Func({?TYPE_SCHEME, Variables, Inner}, FreeVars)
            end;
        {?TYPE_LAMBDA, Parameters, Result} ->
            {NextParameters, NextResult} = map_wfvs_tuple(Parameters, Result, Func, FreeVars, Flag),
            Func({?TYPE_LAMBDA, NextParameters, NextResult}, FreeVars);
        {?TYPE_CONDITION, Type, Constraints} ->
            {NextType, NextConstraints} = map_wfvs_tuple(Type, Constraints, Func, FreeVars, Flag),
            Func({?TYPE_CONDITION, NextType, NextConstraints}, FreeVars);
        {?TYPE_UNION, Ordered, Types} ->
            Func({?TYPE_UNION, Ordered, map_wfvs(Types, Func, FreeVars, Flag)}, FreeVars);
        {?CONSTRAINT_MATCH, Variable, Type} ->
            {NextVariable, NextType} = map_wfvs_tuple(Variable, Type, Func, FreeVars, Flag),
            Func({?CONSTRAINT_MATCH, NextVariable, NextType}, FreeVars);
        {?CONSTRAINT_SUBSETEQ, LeftType, RightType} ->
            {NextLeftType, NextRightType} = map_wfvs_tuple(LeftType, RightType, Func, FreeVars, Flag),
            Func({?CONSTRAINT_SUBSETEQ, NextLeftType, NextRightType}, FreeVars);
        {?CONSTRAINT_JOINABLE, Variables} ->
            Func({?CONSTRAINT_JOINABLE, map_wfvs(Variables, Func, FreeVars, Flag)}, FreeVars);
        {?TYPE_SYMBOL_CALL, Variable, Parameters} ->
            {NextVariable, NextParameters} = map_wfvs_tuple(Variable, Parameters, Func, FreeVars, Flag),
            Func({?TYPE_SYMBOL_CALL, NextVariable, NextParameters}, FreeVars);
        _ ->
            Func(Value, FreeVars)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun map_wfvs/3'.
%% @end
%%-------------------------------------------------------------------------------------------
map_wfvs_tuple(Left, Right, Func, FreeVars, Flag) ->
    LFVS = query:get_free_variables(Left),
    RFVS = query:get_free_variables(Right),
    NextLeft = map_wfvs(Left, Func, util:list_usort_append(RFVS, FreeVars), Flag),
    NextRight = map_wfvs(Right, Func, util:list_usort_append(LFVS, FreeVars), Flag),
    {NextLeft, NextRight}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun map_wfvs/3'.
%% @end
%%-------------------------------------------------------------------------------------------
map_wfvs_list([], _, _, _, _) ->
    [];
map_wfvs_list([Value | Values], Previous, Func, FreeVars, Flag) ->
    FVS = query:get_free_variables(util:list_append(Values, Previous)),
    NextValue = map_wfvs(Value, Func, util:list_usort_append(FreeVars, FVS), Flag),
    [NextValue | map_wfvs_list(Values, [Value | Previous], Func, FreeVars, Flag)].
