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
-module(language).
-author("Gorka Suárez García").
-export([
    parse/1, is_bif/1, get_bif_type/2, get_variables/1, get_free_variables/1,
    get_non_free_variables/1, map/2
]).
-include("language.hrl").

%%===========================================================================================
%% Macros
%%===========================================================================================

-define(USE_SEQUECE_APPEND_TYPE, true).

%%===========================================================================================
%% Parser Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Parses a Mini-Erlang language source file.
%% @param Filename The path of the file to load.
%% @returns The forms if the file is parsed without errors; otherwise {'error', Reason}.
%% @end
%%-------------------------------------------------------------------------------------------
parse(Filename) ->
    Tokens = util:get_text_file_tokens(Filename, fun keywords/1),
    case minierlang:parse(Tokens) of
        {ok, Forms} ->
            Forms;
        {error, {Line, _, Messages}} when is_list(Messages) ->
            {error, lists:append(Messages) ++ " [LINE: " ++ integer_to_list(Line) ++ "]"};
        {error, Reason} ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if an atom is a reserved keyword of the Mini-Erlang language.
%% @param Value The atom to check.
%% @returns 'true' if the atom is a keyword; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
keywords('bnot') -> true;
keywords('div') -> true;
keywords('rem') -> true;
keywords('band') -> true;
keywords('bor') -> true;
keywords('bxor') -> true;
keywords('bsl') -> true;
keywords('bsr') -> true;
keywords('not') -> true;
keywords('and') -> true;
keywords('or') -> true;
keywords('xor') -> true;
keywords('orelse') -> true;
keywords('andalso') -> true;
keywords('fun') -> true;
keywords('let') -> true;
keywords('in') -> true;
keywords('letrec') -> true;
keywords('case') -> true;
keywords('of') -> true;
keywords('end') -> true;
keywords('receive') -> true;
keywords('after') -> true;
keywords('when') -> true;
keywords(_) -> false.

%%===========================================================================================
%% BIF Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if an atom is a built-in function of the Erlang language.
%% @param Value The atom to check.
%% @returns 'true' if the atom is a built-in function; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_bif('is_atom') -> true;
is_bif('is_binary') -> true;
is_bif('is_bitstring') -> true;
is_bif('is_boolean') -> true;
is_bif('is_float') -> true;
is_bif('is_function') -> true;
is_bif('is_integer') -> true;
is_bif('is_list') -> true;
is_bif('is_map') -> true;
is_bif('is_number') -> true;
is_bif('is_pid') -> true;
is_bif('is_port') -> true;
is_bif('is_record') -> true;
is_bif('is_reference') -> true;
is_bif('is_tuple') -> true;
is_bif('abs') -> true;
is_bif('bit_size') -> true;
is_bif('byte_size') -> true;
is_bif('element') -> true;
is_bif('float') -> true;
is_bif('hd') -> true;
is_bif('length') -> true;
is_bif('map_size') -> true;
is_bif('node') -> true;
is_bif('round') -> true;
is_bif('self') -> true;
is_bif('size') -> true;
is_bif('tl') -> true;
is_bif('trunc') -> true;
is_bif('tuple_size') -> true;
is_bif('==') -> true;
is_bif('/=') -> true;
is_bif('=<') -> true;
is_bif('<') -> true;
is_bif('>=') -> true;
is_bif('>') -> true;
is_bif('=:=') -> true;
is_bif('=/=') -> true;
is_bif('+') -> true;
is_bif('-') -> true;
is_bif('*') -> true;
is_bif('/') -> true;
is_bif('bnot') -> true;
is_bif('div') -> true;
is_bif('rem') -> true;
is_bif('band') -> true;
is_bif('bor') -> true;
is_bif('bxor') -> true;
is_bif('bsl') -> true;
is_bif('bsr') -> true;
is_bif('not') -> true;
is_bif('and') -> true;
is_bif('or') -> true;
is_bif('xor') -> true;
is_bif('orelse') -> true;
is_bif('andalso') -> true;
is_bif('++') -> true;
is_bif('--') -> true;
is_bif(_) -> false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the generic type of a built-in function.
%% @param Name The name of the built-in function.
%% @param Size The number of parameters of the built-in function.
%% @returns The generic type of the built-in function.
%% @end
%%-------------------------------------------------------------------------------------------
% General functions:
get_bif_type('abs', 1) ->
    uopnum_type();
get_bif_type('bit_size', 1) ->
    throw(not_supported);
get_bif_type('byte_size', 1) ->
    throw(not_supported);
get_bif_type('element', 2) ->
    type:lambda([type:nznatural(), type:tuple()], type:any());
get_bif_type('float', 1) ->
    type:lambda([type:number()], type:float());
get_bif_type('hd', 1) ->
    type:lambda([type:maybe_improper_list()], type:any());
get_bif_type('length', 1) ->
    type:lambda([type:list()], type:natural());
get_bif_type('map_size', 1) ->
    type:lambda([type:map()], type:natural());
get_bif_type('node', 0) ->
    type:lambda([], type:node());
get_bif_type('node', 1) ->
    type:lambda([type:identifier()], type:node());
get_bif_type('round', 1) ->
    type:lambda([type:number()], type:integer());
get_bif_type('self', 0) ->
    type:lambda([], type:pid());
get_bif_type('size', 1) ->
    type:sequence([
        type:lambda([type:tuple()], type:integer()),
        type:lambda([type:binary()], type:integer())
    ]);
get_bif_type('tl', 1) ->
    type:lambda([type:nelist(type:any(), type:any())], type:any());
get_bif_type('trunc', 1) ->
    type:lambda([type:number()], type:integer());
get_bif_type('tuple_size', 1) ->
    type:lambda([type:tuple()], type:natural());
% Check-type functions:
get_bif_type('is_atom', 1) ->
    type:sequence([
        type:lambda([type:atom()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_binary', 1) ->
    throw(not_supported);
get_bif_type('is_bitstring', 1) ->
    throw(not_supported);
get_bif_type('is_boolean', 1) ->
    type:sequence([
        type:lambda([type:boolean()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_float', 1) ->
    type:sequence([
        type:lambda([type:float()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_function', 1) ->
    type:sequence([
        type:lambda([type:function()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_function', 2) ->
    type:sequence([
        type:lambda([type:function(), type:arity()], type:literal(?TRUE)),
        type:lambda([type:any(), type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_integer', 1) ->
    type:sequence([
        type:lambda([type:integer()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_list', 1) ->
    type:sequence([
        type:lambda([type:maybe_improper_list()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_map', 1) ->
    type:sequence([
        type:lambda([type:map()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_number', 1) ->
    type:sequence([
        type:lambda([type:number()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_pid', 1) ->
    type:sequence([
        type:lambda([type:pid()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_port', 1) ->
    type:sequence([
        type:lambda([type:port()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_record', _) ->
    throw(not_supported);
get_bif_type('is_reference', 1) ->
    type:sequence([
        type:lambda([type:reference()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
get_bif_type('is_tuple', 1) ->
    type:sequence([
        type:lambda([type:tuple()], type:literal(?TRUE)),
        type:lambda([type:any()], type:literal(?FALSE))
    ]);
% Comparer operators:
get_bif_type('==', 2) -> opeq_type();
get_bif_type('/=', 2) -> opneq_type();
get_bif_type('=<', 2) -> opcmp_type();
get_bif_type('<', 2) -> opcmp_type();
get_bif_type('>=', 2) -> opcmp_type();
get_bif_type('>', 2) -> opcmp_type();
get_bif_type('=:=', 2) -> opeq_type();
get_bif_type('=/=', 2) -> opneq_type();
% Arithmetic operators:
get_bif_type('+', 1) -> uopnum_type();
get_bif_type('-', 1) -> uopnum_type();
get_bif_type('+', 2) -> opnum_type();
get_bif_type('-', 2) -> opnum_type();
get_bif_type('*', 2) -> opnum_type();
get_bif_type('/', 2) -> opnum_type();
% Arithmetic integer operators:
get_bif_type('bnot', 1) -> uopint_type();
get_bif_type('div', 2) -> opint_type();
get_bif_type('rem', 2) -> opint_type();
get_bif_type('band', 2) -> opint_type();
get_bif_type('bor', 2) -> opint_type();
get_bif_type('bxor', 2) -> opint_type();
get_bif_type('bsl', 2) -> opint_type();
get_bif_type('bsr', 2) -> opint_type();
% Logical operators:
get_bif_type('not', 1) ->
    type:sequence([
        type:lambda([type:literal(?TRUE)], type:literal(?FALSE)),
        type:lambda([type:literal(?FALSE)], type:literal(?TRUE))
    ]);
get_bif_type('and', 2) ->
    type:sequence([
        type:lambda([type:literal(?TRUE), type:literal(?TRUE)], type:literal(?TRUE)),
        type:lambda([type:literal(?TRUE), type:literal(?FALSE)], type:literal(?FALSE)),
        type:lambda([type:literal(?FALSE), type:literal(?TRUE)], type:literal(?FALSE)),
        type:lambda([type:literal(?FALSE), type:literal(?FALSE)], type:literal(?FALSE))
    ]);
get_bif_type('or', 2) ->
    type:sequence([
        type:lambda([type:literal(?TRUE), type:literal(?TRUE)], type:literal(?TRUE)),
        type:lambda([type:literal(?TRUE), type:literal(?FALSE)], type:literal(?TRUE)),
        type:lambda([type:literal(?FALSE), type:literal(?TRUE)], type:literal(?TRUE)),
        type:lambda([type:literal(?FALSE), type:literal(?FALSE)], type:literal(?FALSE))
    ]);
get_bif_type('xor', 2) ->
    type:sequence([
        type:lambda([type:literal(?TRUE), type:literal(?TRUE)], type:literal(?FALSE)),
        type:lambda([type:literal(?TRUE), type:literal(?FALSE)], type:literal(?TRUE)),
        type:lambda([type:literal(?FALSE), type:literal(?TRUE)], type:literal(?TRUE)),
        type:lambda([type:literal(?FALSE), type:literal(?FALSE)], type:literal(?FALSE))
    ]);
get_bif_type('orelse', 2) ->
    type:sequence([
        type:lambda([type:literal(?TRUE), type:any()], type:literal(?TRUE)),
        type:scheme(
            ['R'],
            type:lambda(
                [type:literal(?FALSE), type:variable('R')],
                type:variable('R')
            )
        )
    ]);
get_bif_type('andalso', 2) ->
    type:sequence([
        type:scheme(
            ['R'],
            type:lambda(
                [type:literal(?TRUE), type:variable('R')],
                type:variable('R')
            )
        ),
        type:lambda([type:literal(?FALSE), type:any()], type:literal(?FALSE))
    ]);
% List operators:
get_bif_type('++', 2) ->
    case ?USE_SEQUECE_APPEND_TYPE of
        true ->
            type:sequence([
                type:scheme(
                    ['R'],
                    type:lambda(
                        [type:nil(), type:variable('R')],
                        type:variable('R')
                    )
                ),
                type:scheme(
                    ['L', 'R'],
                    type:lambda(
                        [type:nelist(type:variable('L')), type:variable('R')],
                        type:nelist(type:variable('L'), type:variable('R'))
                    )
                )
            ]);
        _ ->
            type:scheme(
                ['L', 'R'],
                type:lambda(
                    [type:list(type:variable('L')), type:list(type:variable('R'))],
                    type:list(type:union([type:variable('L'), type:variable('R')]))
                )
            )
    end;
get_bif_type('--', 2) ->
    type:scheme(
        ['L'],
        type:lambda(
            [type:list(type:variable('L')), type:list()],
            type:list(type:variable('L'))
        )
    );
% Default type:
get_bif_type(_, Size) -> type:lambda_none(Size).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic comparer operator type.
%% @returns The generic comparer operator type.
%% @end
%%-------------------------------------------------------------------------------------------
opcmp_type() ->
    type:lambda([type:any(), type:any()], type:boolean()).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic equals comparer operator type.
%% @returns The generic equals comparer operator type.
%% @end
%%-------------------------------------------------------------------------------------------
opeq_type() ->
    type:sequence([
        type:scheme(
            ['V'],
            type:lambda(
                [type:variable('V'), type:variable('V')],
                type:literal('true')
            )
        ),
        type:lambda([type:any(), type:any()], type:literal('false'))
    ]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic not equals comparer operator type.
%% @returns The generic not equals comparer operator type.
%% @end
%%-------------------------------------------------------------------------------------------
opneq_type() ->
    type:lambda([type:any(), type:any()], type:boolean()).
    %%type:sequence([
    %%    type:scheme(
    %%        ['V'],
    %%        type:lambda(
    %%            [type:variable('V'), type:variable('V')],
    %%            type:literal('false')
    %%        )
    %%    ),
    %%    type:lambda([type:any(), type:any()], type:literal('true'))
    %%]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic numeric operator type.
%% @returns The generic numeric operator type.
%% @end
%%-------------------------------------------------------------------------------------------
opnum_type() ->
    type:sequence([
        type:lambda([type:integer(), type:integer()], type:integer()),
        type:lambda([type:float(), type:integer()], type:float()),
        type:lambda([type:integer(), type:float()], type:float()),
        type:lambda([type:float(), type:float()], type:float())
    ]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic numeric unary operator type.
%% @returns The generic numeric unary operator type.
%% @end
%%-------------------------------------------------------------------------------------------
uopnum_type() ->
    type:sequence([
        type:lambda([type:integer()], type:integer()),
        type:lambda([type:float()], type:float())
    ]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic integer operator type.
%% @returns The generic integer operator type.
%% @end
%%-------------------------------------------------------------------------------------------
opint_type() ->
    type:lambda([type:integer(), type:integer()], type:integer()).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the generic integer unary operator type.
%% @returns The generic integer unary operator type.
%% @end
%%-------------------------------------------------------------------------------------------
uopint_type() ->
    type:lambda([type:integer()], type:integer()).

%%===========================================================================================
%% Variable Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the variable names inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
get_variables(Form) ->
    lists:usort(gvars(Form)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the free variable names inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
get_free_variables(Form) ->
    lists:usort(gvars(Form)) -- lists:usort(gnfvars(Form)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the non free variable names inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
get_non_free_variables(Form) ->
    lists:usort(gnfvars(Form)).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the variable names inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
gvars({?VARIABLE_FORM, Name}) ->
    [Name];
gvars({?LIST_FORM, {Head, Tail}}) ->
    util:list_append(gvars(Head), gvars(Tail));
gvars({?LIST_FORM, Expressions}) ->
    gvars(Expressions);
gvars({?TUPLE_FORM, Expressions}) ->
    gvars(Expressions);
gvars({?ABSTRACTION_FORM, {Parameters, Expression}}) ->
    util:list_append(gvars(Parameters), gvars(Expression));
gvars({?APPLICATION_FORM, {Function, Parameters}}) ->
    util:list_append(gvarfn(Function), gvars(Parameters));
gvars({?LET_FORM, {Variable, Value, Expression}}) ->
    util:list_append(util:list_append(gvars(Variable), gvars(Value)), gvars(Expression));
gvars({?LETREC_FORM, {FunDefs, Expression}}) ->
    NFDS = [util:list_append(gvars(L), gvars(R)) || {L, R} <- FunDefs],
    util:list_append(lists:append(NFDS), gvars(Expression));
gvars({?CASE_FORM, {Variable, Clauses}}) ->
    util:list_append(gvars(Variable), gvars(Clauses));
gvars({?RECEIVE_FORM, {Clauses, Variable, Expression}}) ->
    util:list_append(util:list_append(gvars(Clauses), gvars(Variable)), gvars(Expression));
gvars({?CLAUSE_FORM, {Pattern, Guard, Expression}}) ->
    util:list_append(util:list_append(gvars(Pattern), gvars(Guard)), gvars(Expression));
gvars([Form | Forms]) ->
    util:list_append(gvars(Form), gvars(Forms));
gvars(_) ->
    [].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the variable names inside a function name form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
gvarfn(Name) when is_atom(Name) -> [Name];
gvarfn({?FUNNAME_FORM, Name}) -> [Name];
gvarfn({?FUNNAME_FORM, Module, Name}) -> [{Module, Name}];
gvarfn({?VARIABLE_FORM, Name}) -> [Name];
gvarfn(_) -> [].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the non free variable names inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A list with the names obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
gnfvars({?LIST_FORM, {Head, Tail}}) ->
    util:list_append(gnfvars(Head), gnfvars(Tail));
gnfvars({?LIST_FORM, Expressions}) ->
    gnfvars(Expressions);
gnfvars({?TUPLE_FORM, Expressions}) ->
    gnfvars(Expressions);
gnfvars({?ABSTRACTION_FORM, {Parameters, Expression}}) ->
    util:list_append(gvars(Parameters), gnfvars(Expression));
gnfvars({?APPLICATION_FORM, {Function, Parameters}}) ->
    util:list_append(gnfvars(Function), gnfvars(Parameters));
gnfvars({?LET_FORM, {Variable, Value, Expression}}) ->
    util:list_append(util:list_append(gvars(Variable), gnfvars(Value)), gnfvars(Expression));
gnfvars({?LETREC_FORM, {FunDefs, Expression}}) ->
    NFDS = [util:list_append(gvars(L), gnfvars(R)) || {L, R} <- FunDefs],
    util:list_append(lists:append(NFDS), gnfvars(Expression));
gnfvars({?CASE_FORM, {Variable, Clauses}}) ->
    util:list_append(gnfvars(Variable), gnfvars(Clauses));
gnfvars({?RECEIVE_FORM, {Clauses, Variable, Expression}}) ->
    util:list_append(util:list_append(gnfvars(Clauses), gnfvars(Variable)), gnfvars(Expression));
gnfvars({?CLAUSE_FORM, {Pattern, Guard, Expression}}) ->
    util:list_append(util:list_append(gvars(Pattern), gnfvars(Guard)), gnfvars(Expression));
gnfvars([Form | Forms]) ->
    util:list_append(gnfvars(Form), gnfvars(Forms));
gnfvars(_) ->
    [].

%%===========================================================================================
%% Transform Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps an operation with a language expression form.
%% @param Form The language expression form to map.
%% @param Operation The operation to use in the map.
%% @returns The new language expression form obtained the map.
%% @end
%%-------------------------------------------------------------------------------------------
map([], _) ->
    [];
map([Form | Forms], Operation) ->
    [map(Form, Operation) | map(Forms, Operation)];
map({?LITERAL_FORM, Value}, Operation) ->
    Operation({?LITERAL_FORM, Value});
map({?VARIABLE_FORM, Name}, Operation) ->
    Operation({?VARIABLE_FORM, Name});
map({?LIST_FORM, {Head, Tail}}, Operation) ->
    Operation({?LIST_FORM, {map(Head, Operation), map(Tail, Operation)}});
map({?LIST_FORM, Expressions}, Operation) ->
    Operation({?LIST_FORM, map(Expressions, Operation)});
map({?TUPLE_FORM, Expressions}, Operation) ->
    Operation({?TUPLE_FORM, map(Expressions, Operation)});
map({?ABSTRACTION_FORM, {Parameters, Expression}}, Operation) ->
    Operation({?ABSTRACTION_FORM, {map(Parameters, Operation), map(Expression, Operation)}});
map({?APPLICATION_FORM, {Function, Parameters}}, Operation) ->
    Operation({?APPLICATION_FORM, {map(Function, Operation), map(Parameters, Operation)}});
map({?LET_FORM, {Variable, Value, Expression}}, Operation) ->
    Operation({?LET_FORM, {map(Variable, Operation), map(Value, Operation), map(Expression, Operation)}});
map({?LETREC_FORM, {FunDefs, Expression}}, Operation) ->
    NFDS = [{map(L, Operation), map(R, Operation)} || {L, R} <- FunDefs],
    Operation({?LETREC_FORM, {NFDS, map(Expression, Operation)}});
map({?CASE_FORM, {Variable, Clauses}}, Operation) ->
    Operation({?CASE_FORM, {map(Variable, Operation), map(Clauses, Operation)}});
map({?RECEIVE_FORM, {Clauses, Variable, Expression}}, Operation) ->
    Operation({?RECEIVE_FORM, {map(Clauses, Operation), map(Variable, Operation), map(Expression, Operation)}});
map({?CLAUSE_FORM, {Pattern, Guard, Expression}}, Operation) ->
    Operation({?CLAUSE_FORM, {map(Pattern, Operation), map(Guard, Operation), map(Expression, Operation)}});
map({?FUNNAME_FORM, FunName}, Operation) ->
    Operation({?FUNNAME_FORM, map(FunName, Operation)});
map({?FUNNAME_FORM, FunModule, FunName}, Operation) ->
    Operation({?FUNNAME_FORM, map(FunModule, Operation), map(FunName, Operation)});
map(Form, Operation) ->
    Operation(Form).
