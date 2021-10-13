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
-module(analyze).
-author("Gorka Suárez García").
-export([get_constraints/1]).
-include("type.hrl").
-include("language.hrl").

%%===========================================================================================
%% Constant Values
%%===========================================================================================

-define(FLAG_ITV,      itv      ).
-define(FLAG_ITV_PLUS, itv_plus ).

%%===========================================================================================
%% Constraints Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the constraints inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns An annotated type is obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
get_constraints([]) ->
    [];
get_constraints([Form | Forms]) ->
    [get_constraints(Form) | get_constraints(Forms)];
get_constraints({?LITERAL_FORM, Value}) ->
    % Get the value as literal type:
    [type:pair(type:literal(Value))];
get_constraints({?VARIABLE_FORM, Name}) ->
    % Get the name as variable type:
    [type:pair(type:variable(Name, true))];
get_constraints({?LIST_FORM, {Head, Tail}}) ->
    % Get the subexpressions types and merge them into a nelist:
    type:mergex(
        get_constraints(Head),
        get_constraints(Tail),
        fun([HT, TT]) ->
            type:nelist(HT, TT)
        end
    );
get_constraints({?LIST_FORM, Expressions}) ->
    % Get the subexpressions types and merge them into a nelist:
    Aux = type:mergex(
        [get_constraints(E) || E <- Expressions],
        fun(Types) ->
            type:nelist(type:union(Types))
        end
    ),
    Aux;
get_constraints({?TUPLE_FORM, Expressions}) ->
    % Get the subexpressions types and merge them into a tuple:
    type:merge([get_constraints(E) || E <- Expressions]);
get_constraints(Form = {?ABSTRACTION_FORM, {Parameters, Expression}}) ->
    % Get the subexpressions constraints:
    ATS = type:mergex(
        [get_constraints(E) || E <- [Expression | Parameters]],
        fun([TR | TPS]) -> {TPS, TR} end
    ),
    % Get the free variables inside the abstraction and make a function
    % that obtains the non free type variables of a lambda type:
    GSV = create_get_scheme_variables(Form, ?FLAG_ITV_PLUS),
    % Make a list of lambdas to build then a list of schemes, where all
    % the non free type variables will be closed for the final type:
    Lambdas = [type:lambda(TPS, type:condition(TR, CS)) || {_, {TPS, TR}, CS} <- ATS],
    Schemes = lists:map(
        fun(L) ->
            NS = GSV(L),
            NL = type:set_linked_flag(L, NS, false),
            type:scheme(NS, NL)
        end,
        Lambdas
    ),
    % Return the union of the schemes:
    [type:pair(type:sequence(Schemes))];
get_constraints({?APPLICATION_FORM, {Function, Parameters}}) ->
    % Get the subexpressions constraints:
    NAT = get_constraints_from_name(Function),
    ATS = type:mergex(
        [NAT | [get_constraints(P) || P <- Parameters]],
        fun([TN | TPS]) -> {TN, TPS} end
    ),
    % Make a fresh variable that will be the return type, then insert
    % in all the returned annotated types the symbolic restrictions:
    RT = data:fresh_variable(),
    lists:map(
        fun({_, {TN, TPS}, CS}) ->
            RCS = constraint:match(RT, type:symbol_call(TN, TPS)),
            type:pair(RT, constraint:insert(CS, RCS))
        end,
        ATS
    );
get_constraints({?LET_FORM, {Variable, Value, Expression}}) ->
    % Get the subexpressions constraints:
    {_, XT, _} = hd(get_constraints(Variable)),
    Name= type:get_variable_name(XT),
    ATS = type:mergex(
        get_constraints(Value),
        get_constraints(Expression),
        fun([VT, ET]) ->
            {ET, constraint:match(XT, VT)}
        end
    ),
    % Return the body expression type and the merged constraints
    % with a restriction over the variable:
    lists:map(
        fun({_, {ET, XVC}, CS}) ->
            ET2 = type:set_linked_flag(ET, Name, false),
            CS2 = type:set_linked_flag(constraint:insert(CS, XVC), Name, false),
            type:pair(ET2, CS2)
        end,
        ATS
    );
get_constraints({?LETREC_FORM, {FunDefs, Expression}}) ->
    % Get the subexpressions constraints:
    VFTCS = [{hd(get_constraints(V)), hd(get_constraints(F))} || {V, F} <- FunDefs],
    EAT = get_constraints(Expression),
    % Merge all the definition constraints and add a restriction over
    % the variables with the abstraction types:
    CS = lists:foldl(
        fun({{_, VT, _}, {_, FT, _}}, Accum) ->
            constraint:insert_match(Accum, VT, FT)
        end,
        constraint:conjunction(),
        VFTCS
    ),
    Names = [N || {{_, {_, N, _}, _}, _} <- VFTCS],
    % Return the body expression type and merge all the constraints:
    lists:map(
        fun({_, ET, EC}) ->
            ET2 = type:set_linked_flag(ET, Names, false),
            EC2 = type:set_linked_flag(EC, Names, false),
            CS2 = type:set_linked_flag(CS, Names, false),
            type:pair(ET2, constraint:merge(CS2, EC2))
        end,
        EAT
    );
get_constraints({?CASE_FORM, {Variable, Clauses}}) ->
    % Get the subexpressions constraints:
    {_, XT, _} = hd(get_constraints(Variable)),
    CAT = lists:append([get_constraints(C) || C <- Clauses]),
    % Insert in all the clauses returned constraints a restriction
    % to match the variable with the pattern type of the clause:
    lists:map(
        fun({_, {PT, BT}, CS}) ->
            Names = query:get_variables(PT),
            BT2 = type:set_linked_flag(BT, Names, false),
            CS2 = constraint:insert_match(CS, XT, PT),
            CS3 = type:set_linked_flag(CS2, Names, false),
            type:pair(BT2, CS3)
        end,
        CAT
    );
get_constraints({?RECEIVE_FORM, {Clauses, Variable, Expression}}) ->
    % Get the subexpressions constraints:
    CAT = lists:append([get_constraints(C) || C <- Clauses]),
    {_, XT, _} = hd(get_constraints(Variable)),
    EAT = get_constraints(Expression),
    % Insert in all the clauses returned constraints a restriction
    % to check the time variable type of the after clause:
    CAT2 = lists:map(
        fun({_, {PT, BT}, CS}) ->
            Names = query:get_variables(PT),
            BT2 = type:set_linked_flag(BT, Names, false),
            CS2 = constraint:insert_subseteq(CS, XT, type:timeout()),
            CS3 = type:set_linked_flag(CS2, Names, false),
            type:pair(BT2, CS3)
        end,
        CAT
    ),
    EAT2 = lists:map(
        fun({_, ET, EC}) ->
            EC2 = constraint:insert_subseteq(EC, XT, type:integer()),
            type:pair(ET, EC2)
        end,
        EAT
    ),
    CAT2 ++ EAT2;
get_constraints({?CLAUSE_FORM, {Pattern, Guard, Expression}}) ->
    % Get the subexpressions constraints:
    {_, PT, _} = hd(get_constraints(Pattern)),
    ATS = type:mergex(
        get_constraints(Guard),
        get_constraints(Expression),
        fun([GT, ET]) ->
            {PT, ET, constraint:subseteq(type:literal(?TRUE), GT)}
        end
    ),
    % Return the pattern type (to be used in the case form), the body expression
    % type, and the merged constraints with the guard restriction inserted:
    [type:pair({P, E}, constraint:insert(CS, C)) || {_, {P, E, C}, CS} <- ATS];
get_constraints(_) ->
    throw(invalid_form).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the constraints inside a expression form.
%% @param Form The language expression form to analyze.
%% @returns A pair obtained from the form.
%% @end
%%-------------------------------------------------------------------------------------------
get_constraints_from_name(Name) when is_atom(Name) ->
    % Get the variable as type and an empty conjunction:
    [type:pair(type:variable(Name, true))];
get_constraints_from_name({?FUNNAME_FORM, Name}) ->
    % Get the variable as type and an empty conjunction:
    [type:pair(type:variable(Name, true))];
get_constraints_from_name({?FUNNAME_FORM, Module, Name}) ->
    % Get the variable as type and an empty conjunction:
    [type:pair(type:variable({Module, Name}, true))];
get_constraints_from_name({?VARIABLE_FORM, Name}) ->
    % Get the variable as type and an empty conjunction:
    [type:pair(type:variable(Name, true))];
get_constraints_from_name(_) ->
    throw(invalid_form).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Creates a function to get the scheme variables.
%% @param Form The language expression form to analyze.
%% @param Flag The type of created function flag.
%% @returns The get scheme variables function.
%% @end
%%-------------------------------------------------------------------------------------------
create_get_scheme_variables(_, ?FLAG_ITV) ->
    fun query:get_instantiable_variables/1;
create_get_scheme_variables(_, ?FLAG_ITV_PLUS) ->
    fun(Victim) ->
        ITVS = query:get_instantiable_variables(Victim),
        ULVS = query:get_unlinked_variables(Victim),
        util:list_usort_append(ITVS, ULVS)
    end;
create_get_scheme_variables(Form, _) ->
    FV = language:get_free_variables(Form),
    fun(L) -> query:get_variables(L) -- FV end.
