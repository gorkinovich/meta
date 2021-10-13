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
-module(tests).
-author("Gorka Suárez García").
-compile(nowarn_export_all).
-compile(export_all).
-include("data.hrl").
-include("language.hrl").
-include("reduce.hrl").
-include("type.hrl").

%%===========================================================================================

execute() ->
    %data:set_show_debug(false),
    io:format("I'm just playing games~n"),
    io:format("I know that's plastic love~n"),
    io:format("Dance to the plastic beat~n"),
    io:format("Another morning comes~n"),
    %data:set_show_debug(true),
    plastic_love.

%%===========================================================================================

widening_change_type() ->
    T0 = {'SCHEME',['A','B','C','D'],{'LAMBDA',
        [{'SET',nelist,[{'VAR','A',false},{'SET',nelist,[{'VAR','B',false},{'SET',nelist,[{'VAR','C',false},{'LIT',[]}]}]}]},{'VAR','D',false}],
        {'SET',nelist,[{'VAR','C',false},{'SET',nelist,[{'VAR','B',false},{'SET',nelist,[{'VAR','A',false},{'VAR','D',false}]}]}]}}},
    T1 = {'SCHEME',['A','B','C','D'],{'LAMBDA',
        [{'SET',tuple,[{'VAR','A',false},{'SET',tuple,[{'VAR','B',false},{'SET',tuple,[{'VAR','C',false},{'LIT',[]}]}]}]},{'VAR','D',false}],
        {'SET',tuple,[{'VAR','C',false},{'SET',tuple,[{'VAR','B',false},{'SET',tuple,[{'VAR','A',false},{'VAR','D',false}]}]}]}}},
    widening_change_type(T0),
    widening_change_type(T1).

widening_change_type(T0) ->
    T1 = widening:change_type(T0),
    T2 = widening:change_type(T1),
    T3 = widening:change_type(T2),
    T4 = widening:change_type(T3),
    io:format("T0(~p) := ~s~n", [query:get_height(T0), text:type(T0)]),
    io:format("T1(~p) := ~s~n", [query:get_height(T1), text:type(T1)]),
    io:format("T2(~p) := ~s~n", [query:get_height(T2), text:type(T2)]),
    io:format("T3(~p) := ~s~n", [query:get_height(T3), text:type(T3)]),
    io:format("T4(~p) := ~s~n~n", [query:get_height(T4), text:type(T4)]).

%%===========================================================================================

poly_subseteq_v4() ->
    S0 = {'SCHEME',['YS'],{'LAMBDA',[{'LIT',[]},{'VAR','YS',false}],{'VAR','YS',false}}},
    S1 = {'SCHEME',['X','YS'],{'LAMBDA',
        [{'SET',nelist,[{'VAR','X',false},{'LIT',[]}]},{'VAR','YS',false}],
        {'SET',nelist,[{'VAR','X',false},{'VAR','YS',false}]}}},
    S2 = {'SCHEME',[40,'X','YS'],{'LAMBDA',
        [{'SET',nelist,[{'VAR','X',false},{'SET',nelist,[{'VAR',40,false},{'LIT',[]}]}]},{'VAR','YS',false}],
        {'SET',nelist,[{'VAR',40,false},{'SET',nelist,[{'VAR','X',false},{'VAR','YS',false}]}]}}},
    S3 = {'SCHEME',[45,46,'X','YS'],{'LAMBDA',
        [{'SET',nelist,[{'VAR','X',false},{'SET',nelist,[{'VAR',46,false},{'SET',nelist,[{'VAR',45,false},{'LIT',[]}]}]}]},{'VAR','YS',false}],
        {'SET',nelist,[{'VAR',45,false},{'SET',nelist,[{'VAR',46,false},{'SET',nelist,[{'VAR','X',false},{'VAR','YS',false}]}]}]}}},
    T1 = {'UNION',true,[S0,S1]},
    T2 = {'UNION',true,[S0,S1,S2]},
    T3 = {'UNION',true,[S0,S1,S2,S3]},
    io:format("============================================================~n~n"),
    io:format("S0 := ~s~n", [text:type(S0)]),
    io:format("S1 := ~s~n", [text:type(S1)]),
    io:format("S2 := ~s~n", [text:type(S2)]),
    io:format("S3 := ~s~n~n", [text:type(S3)]),
    io:format("============================================================~n~n"),
    io:format("T1 := ~s~n", [text:type(T1)]),
    io:format("T2 := ~s~n", [text:type(T2)]),
    io:format("T3 := ~s~n~n", [text:type(T3)]),
    io:format("============================================================~n~n"),
    io:format("S2 <= S1: ~p~n~n", [poly:subseteq(S2, S1)]),
    io:format("S3 <= S2: ~p~n~n", [poly:subseteq(S3, S2)]),
    io:format("T2 <= T1: ~p~n~n", [poly:subseteq(T2, T1)]),
    io:format("T3 <= T2: ~p~n~n", [poly:subseteq(T3, T2)]),
    io:format("============================================================~n~n").

%%===========================================================================================

poly_subseteq_v3() ->
    L0 = type:nelist(type:variable('A1')),
    L1 = type:nelist(type:variable('A2'), type:nelist(type:variable('B2'))),
    T0 = type:lambda([type:variable('A1'), L0], L0),
    T1 = type:lambda([type:variable('A2'), L1], L1),
    io:format("T1 <= T0: ~p~n~n", [poly:subseteq(T1, T0)]),
    io:format("T0 <= T1: ~p~n~n", [poly:subseteq(T0, T1)]).

%%===========================================================================================

poly_subseteq_v2() ->
    L0 = type:nelist(type:variable('X')),
    L1 = type:nelist(type:variable('X'), type:nelist(type:variable('Y'))),
    F0 = type:lambda([type:nil()], type:integer()),
    S0 = type:scheme(['X'], type:lambda([L0], type:integer())),
    S1 = type:scheme(['X', 'Y'], type:lambda([L1], type:integer())),
    S2 = type:scheme(['X'], type:lambda([L0], L0)),
    S3 = type:scheme(['X', 'Y'], type:lambda([L1], L1)),
    T0 = type:sequence([F0, S0]),
    T1 = type:sequence([F0, S0, S1]),
    io:format("============================================================~n~n"),
    io:format("L0 := ~s~n", [text:type(L0)]),
    io:format("L1 := ~s~n", [text:type(L1)]),
    io:format("T0 := ~s~n", [text:type(S0)]),
    io:format("T1 := ~s~n", [text:type(S1)]),
    io:format("T2 := ~s~n", [text:type(S2)]),
    io:format("T3 := ~s~n", [text:type(S3)]),
    io:format("============================================================~n~n"),
    io:format("S1 <= S0: ~p~n~n", [poly:subseteq(S1, S0)]), % true
    io:format("S3 <= S2: ~p~n~n", [poly:subseteq(S3, S2)]), % true
    io:format("T1 <= T0: ~p~n~n", [poly:subseteq(T1, T0)]), % true
    io:format("============================================================~n~n").

%%===========================================================================================

poly_subseteq_v1() ->
    L0 = type:lambda([type:literal([])], type:literal(0)),
    L1 = type:scheme(['X'], type:lambda([type:nelist(type:variable('X'))], type:integer())),
    L2 = type:scheme(['X', 'Y'], type:lambda([type:nelist(type:variable('X'),
        type:nelist(type:variable('Y')))], type:integer())),
    T1 = type:sequence([L0, L1]),
    T2 = type:sequence([L0, L1, L2]),
    VA = type:tuple([type:any(), type:any()]),
    V0 = type:tuple([type:variable('A'), type:variable('B')]),
    V1 = type:condition(V0, [constraint:match(type:variable('A'), type:variable('B'))]),
    V2 = type:condition(V0, [constraint:match(type:variable('A'), type:variable('B')),
        constraint:subseteq(type:variable('A'), type:integer())]),
    V3 = type:condition(V0, [constraint:subseteq(type:variable('A'), type:integer()),
        constraint:subseteq(type:variable('B'), type:integer())]),
    V4 = type:nelist(type:condition(V0, [constraint:match(type:variable('A'), type:variable('B'))])),
    V5 = type:condition(type:nelist(V0), [constraint:match(type:variable('A'), type:variable('B'))]),
    V6 = type:nelist(type:condition(type:variable('A'),
        [constraint:match(type:variable('A'), type:tuple([type:variable('B'), type:variable('C')])),
            constraint:match(type:variable('B'), type:variable('C'))])),
    V7 = type:condition(type:nelist(type:variable('A')),
        [constraint:match(type:variable('A'), type:tuple([type:variable('B'), type:variable('C')])),
            constraint:match(type:variable('B'), type:variable('C')),
            constraint:match(type:variable('C'), type:variable('D')),
            constraint:match(type:variable('D'), type:variable('B'))]),
    io:format("============================================================~n~n"),
    io:format("L0 := ~s~n", [text:type(L0)]),
    io:format("L1 := ~s~n", [text:type(L1)]),
    io:format("L2 := ~s~n", [text:type(L2)]),
    io:format("T1 := ~s~n", [text:type(T1)]),
    io:format("T2 := ~s~n", [text:type(T2)]),
    io:format("V0 := ~s~n", [text:type(V0)]),
    io:format("V1 := ~s~n", [text:type(V1)]),
    io:format("V2 := ~s~n", [text:type(V2)]),
    io:format("V3 := ~s~n", [text:type(V3)]),
    io:format("V4 := ~s~n", [text:type(V4)]),
    io:format("V5 := ~s~n", [text:type(V5)]),
    io:format("V6 := ~s~n", [text:type(V6)]),
    io:format("V7 := ~s~n~n", [text:type(V7)]),
    io:format("============================================================~n~n"),
    data:set_show_debug(false),
    io:format("L1 <= L2: ~p~n~n", [poly:subseteq(L1, L2)]), % false
    io:format("L2 <= L1: ~p~n~n", [poly:subseteq(L2, L1)]), % true
    io:format("T1 <= T0: ~p~n~n", [poly:subseteq(T1, L0)]), % false
    io:format("T0 <= T1: ~p~n~n", [poly:subseteq(L0, T1)]), % true
    io:format("T2 <= T1: ~p~n~n", [poly:subseteq(T2, T1)]), % true
    io:format("T1 <= T2: ~p~n~n", [poly:subseteq(T1, T2)]), % true
    io:format("V0 <= V1: ~p~n~n", [poly:subseteq(V0, V1)]), % false
    io:format("V1 <= V0: ~p~n~n", [poly:subseteq(V1, V0)]), % true
    io:format("V0 <= V2: ~p~n~n", [poly:subseteq(V0, V2)]), % false
    io:format("V2 <= V0: ~p~n~n", [poly:subseteq(V2, V0)]), % true
    io:format("V0 <= V3: ~p~n~n", [poly:subseteq(V0, V3)]), % false
    io:format("V3 <= V0: ~p~n~n", [poly:subseteq(V3, V0)]), % true
    io:format("V1 <= V2: ~p~n~n", [poly:subseteq(V1, V2)]), % false
    io:format("V2 <= V1: ~p~n~n", [poly:subseteq(V2, V1)]), % true
    io:format("V1 <= V3: ~p~n~n", [poly:subseteq(V1, V3)]), % false
    io:format("V3 <= V1: ~p~n~n", [poly:subseteq(V3, V1)]), % false
    io:format("V4 <= V5: ~p~n~n", [poly:subseteq(V4, V5)]), % true
    io:format("V5 <= V4: ~p~n~n", [poly:subseteq(V5, V4)]), % false
    io:format("============================================================~n~n"),
    data:set_show_debug(false),
    io:format("V6 <= V7: ~p~n~n", [poly:subseteq(V6, V7)]), % true
    io:format("V7 <= V6: ~p~n~n", [poly:subseteq(V7, V6)]), % false
    io:format("VA <= V1: ~p~n~n", [poly:subseteq(VA, V1)]), % false
    io:format("V1 <= VA: ~p~n~n", [poly:subseteq(V1, VA)]), % true
    io:format("============================================================~n~n").

%%===========================================================================================

normalize_recursive() ->
    P0 = type:lambda([type:variable('A')], type:condition(type:variable('B'),
        [constraint:match(type:variable('A'), type:variable('C')),
            constraint:match(type:variable('B'), type:variable('D'))])),
    P1 = type:nelist(type:variable('E'), type:nelist(type:variable('F'))),
    P2 = type:condition(type:nelist(type:variable('G'), type:nelist(type:variable('H'))), [
        constraint:subseteq(type:variable('E'), type:variable('A')),
        constraint:subseteq(type:variable('F'), type:variable('C')),
        constraint:subseteq(type:variable('G'), type:variable('B')),
        constraint:subseteq(type:variable('H'), type:variable('D'))
    ]),
    T1 = type:lambda([P0, P1], P2),
    T2 = type:nelist(type:condition(type:tuple([type:variable('A'), type:variable('B')]), [
        constraint:match(type:variable('A'), type:variable('B')),
        constraint:match(type:variable('C'), type:variable('B')),
        constraint:subseteq(type:variable('C'), type:integer())
    ])),
    % (($12) -> ($13 when $12 := $22, $13 := $23), nelist($14, nelist($24, []))) ->
    % (nelist($15, nelist($25, [])) when $14 <= $12, $15 <= $13, $24 <= $22, $25 <= $23))
    io:format("T1 := ~s~n", [text:type(T1)]),
    io:format("T1 := ~s~n", [text:type(normalize:recursive(T1))]),
    io:format("T2 := ~s~n", [text:type(T2)]),
    io:format("T2 := ~s~n", [text:type(normalize:recursive(T2))]).

%%===========================================================================================

query_variables() ->
    X1 = type:variable('A'),
    X2 = type:variable('B'),
    X3 = type:variable('C'),
    X4 = type:variable('D'),
    X5 = type:variable('E'),
    X6 = type:variable('F'),
    X7 = type:variable('G'),
    X8 = type:variable('H'),
    X9 = type:variable('I'),
    X10 = type:variable('J'),
    X11 = type:variable('K'),
    W1 = type:condition(type:tuple([type:nelist(type:tuple([X1, X2, X6])), X2, X7]),
        [
            constraint:match(X1, X3), constraint:match(X2, X4),
            constraint:match(X10, X8),
            constraint:match(X11, X9),
            constraint:subseteq(X8, X4),
            constraint:subseteq(X4, X9),
            constraint:subseteq(X3, type:integer()),
            constraint:subseteq(X4, type:atom()),
            constraint:subseteq(X1, type:literal(1)),
            constraint:subseteq(X5, type:float())
        ]),
    W2 = query:variables(W1),
    W3 = query:variables_simple(W1),
    io:format("TYPE:~n~s~n~n", [text:type(W1)]),
    io:format("INFO:~n~p~n~n", [W2]),
    io:format("INFO:~n~p~n~n", [W3]).

%%===========================================================================================

mono_subseteq() ->
    T0 = type:nelist(type:any()),
    T1 = type:nelist(type:any(), type:nelist(type:any())),
    io:format("T0:~n~p~n~n", [T0]),
    io:format("T1:~n~p~n~n", [T1]),
    io:format("T0:~n~s~n~n", [text:type(T0)]),
    io:format("T1:~n~s~n~n", [text:type(T1)]),
    io:format("T0 <= T1: ~p~n~n", [mono:subseteq(T0, T1)]),
    io:format("T1 <= T0: ~p~n~n", [mono:subseteq(T1, T0)]).

%%===========================================================================================

ground_get_type() ->
    Victim = type:pair(
        type:tuple([type:variable("A"), type:variable("B"), type:variable("C")]),
        [
            constraint:match(type:variable("A"), type:variable("B")),
            constraint:match(type:variable("A"), type:integer()),
            constraint:match(type:variable("E"), type:integer()),
            constraint:match(type:variable("F"), type:literal(1)),
            constraint:match(type:variable("C"),
                type:scheme(["A","D"],
                    type:lambda(
                        [type:variable("A")],
                        type:condition(
                            type:variable("D"),
                            [
                                constraint:match(type:variable("E"), type:variable("F")),
                                constraint:subseteq(type:variable("G"), type:variable("E")),
                                constraint:subseteq(type:variable("G"), type:variable("H")),
                                constraint:subseteq(type:literal(1), type:variable("G")),
                                constraint:subseteq(type:variable("D"),
                                    type:tuple([type:variable("G"), type:variable("I")])),
                                constraint:match(type:variable("H"), type:literal(1)),
                                constraint:subseteq(type:variable("I"), type:variable("J")),
                                constraint:subseteq(type:variable("J"), type:variable("K")),
                                constraint:subseteq(type:variable("K"), type:variable("I")),
                                constraint:subseteq(type:variable("K"), type:variable("L")),
                                constraint:subseteq(type:variable("L"), type:atom()),
                                constraint:match(type:variable("G"), type:variable("A"))
                            ]
                        )
                    )))
        ]
    ),
    Result = ground:get_type(Victim),
    io:format("VICTIM:~n~p~n~n", [Victim]),
    io:format("RESULT:~n~p~n~n", [Result]),
    io:format("VICTIM:~n~s~n~n", [text:type(Victim)]),
    io:format("RESULT:~n~s~n~n", [text:type(Result)]).

%%===========================================================================================

language_variables(Filename) ->
    parse(Filename,
        fun(Forms) ->
            io:format("[=== Forms Tests ===]~n"),
            io:format("~p~n", [Forms]),
            io:format("[=== Lang Tests ===]~n"),
            io:format("VARS: ~p~n", [language:get_variables(Forms)]),
            io:format("FVARS: ~p~n", [language:get_free_variables(Forms)]),
            io:format("NFVARS: ~p~n", [language:get_non_free_variables(Forms)]),
            io:format("[=== Map Tests ===]~n"),
            language:map(Forms, fun(Form) -> io:format("~p~n", [Form]), Form end),
            io:format("[=== End Tests ===]~n")
        end
    ).

%%===========================================================================================

analyze_get_constraints(Filename) ->
    parse(Filename,
        fun(Forms) ->
            io:format("[=== Constraints ===]~n"),
            lists:map(
                fun(Pair) ->
                    io:format("~p~n~n", [Pair]),
                    io:format("TYPE:~n~s~n~n", [text:type(Pair)])
                end,
                analyze:get_constraints(Forms)
            ),
            io:format("[=== End Tests ===]~n")
        end
    ).

%%===========================================================================================

reduce_execute(Filename) ->
    parse(Filename,
        fun(Forms) ->
            io:format("[=== Constraints ===]~n"),
            lists:map(
                fun(Pair) ->
                    io:format("TYPE:~n~s~n~n", [text:type(Pair)]),
                    io:format("[=== Transformation ===]~n"),
                    NewPair = reduce:execute(Pair),
                    io:format("TYPE:~n~s~n~n", [text:type(NewPair)])
                end,
                analyze:get_constraints(Forms)
            ),
            io:format("[=== End Tests ===]~n")
        end
    ).

%%===========================================================================================

parse(Filename, Function) ->
    io:format("FILE: ~s~n", [Filename]),
    case language:parse(Filename) of
        {error, Reason} -> io:format("ERROR: ~p~n", [Reason]);
        Forms -> Function(Forms)
    end.

%%===========================================================================================

table() ->
    table(1, [{a, []}, {b, [c]}, {c, [d,e]}, {d, [b]}, {e, [c]}, {f, [f]}, {g, [a,b]}, {h, []}]),
    table(2, [{a, []}, {b, [b]}, {c, [a]}, {d, [b]}]),
    table(3, [{a, []}, {b, [c,e]}, {c, [c,d]}, {d, [c,d]}, {e, [e]}]),
    table(4, [{a, []}, {b, [c]}, {c, [c]}]),
    plastic_love.

table(N, V) ->
    D = table:sort_dependencies(V),
    io:format("Value ~p:~n~p~n~nResult ~p:~n~p~n~n~n", [N, V, N, D]).

%%===========================================================================================

tools() ->
    io:format("[=== Begin Test ===]~n"),
    V1 = type:variable('X'),
    V2 = type:variable('Y'),
    V3 = type:variable('Z', true),
    L1 = type:literal([]),
    L2 = type:literal(123),
    T1 = type:nelist(V3, L1),
    T2 = type:tuple([V1, V2, T1]),
    T3 = type:tuple([V3, L2]),
    C1 = type:condition(T2, [
        constraint:match(V1, V2),
        constraint:subseteq(V1, type:number())
    ]),
    F1 = type:lambda([V1, type:boolean()], C1),
    S1 = type:scheme(['X', 'Y'], F1),
    F2 = type:lambda([type:atom()], T3),
    S2 = type:scheme([], F2),
    Q1 = type:sequence([S1, S2]),
    io:format("VICTIM:~n~p~n~n", [Q1]),
    io:format("TO_STRING:~n~s~n~n", [text:type(Q1)]),
    %identity_tools_map(Q1),
    %identity_tools_mapex(Q1, ok),
    io:format("[=== End Test ===]~n").

%%===========================================================================================

merantype() ->
    io:format("[=== Begin Test ===]~n"),
    V1 = type:variable('A'),
    V2 = type:variable('B'),
    V3 = type:variable('C'),
    V4 = type:variable('D'),
    V5 = type:variable('E'),
    V6 = type:variable('F'),
    V7 = type:variable('G'),
    V8 = type:variable('H'),
    V9 = type:variable('I'),
    AT1 = [
        type:pair(V1, [constraint:subseteq(V1, type:literal(1))]),
        type:pair(V2, [constraint:subseteq(V2, type:literal(2))]),
        type:pair(V3, [constraint:subseteq(V3, type:literal(3))])
    ],
    AT2 = [
        type:pair(V4, [constraint:subseteq(V4, type:literal(4))]),
        type:pair(V5, [constraint:subseteq(V5, type:literal(5))]),
        type:pair(V6, [constraint:subseteq(V6, type:literal(6))])
    ],
    AT3 = [
        type:pair(V7, [constraint:subseteq(V7, type:literal(7))]),
        type:pair(V8, [constraint:subseteq(V8, type:literal(8))]),
        type:pair(V9, [constraint:subseteq(V9, type:literal(9))])
    ],
    ATS = [AT1, AT2, AT3],
    MAT = type:merge(ATS),
    io:format("VICTIMS:~n~p~n~n", [ATS]),
    io:format("TO_STRING:~n~s~n~n", [text:type(AT1)]),
    io:format("TO_STRING:~n~s~n~n", [text:type(AT2)]),
    io:format("TO_STRING:~n~s~n~n", [text:type(AT3)]),
    io:format("MERGE:~n~s~n~n", [text:type(MAT)]),
    io:format("[=== End Test ===]~n").

%%===========================================================================================

listproduct() ->
    io:format("[=== Begin Test ===]~n"),
    L1 = [[1,2,3],[4,5,6],[7,8,9]],
    L2 = [[1,2,3],[],[4,5,6],[7,8,9]],
    F1 = util:list_product(L1),
    F2 = util:list_product(L2),
    io:format("L1: ~p~n", [L1]),
    io:format("L2: ~p~n~n", [L2]),
    io:format("F1: ~p~n", [F1]),
    io:format("F2: ~p~n~n", [F2]),
    io:format("F2: ~p~n~n", [F1 =:= F2]),
    io:format("[=== End Test ===]~n").

%%===========================================================================================

getconstraints() ->
    io:format("[=== Begin Test ===]~n"),
    L1 = {?LITERAL_FORM, []},
    L2 = {?LITERAL_FORM, 123},
    L3 = {?LITERAL_FORM, 'true'},
    VX = {?VARIABLE_FORM, 'X'},
    VY = {?VARIABLE_FORM, 'Y'},
    EL1 = {?LIST_FORM, {L2, L1}},
    EL2 = {?LIST_FORM, [VX, VY, L2]},
    ET1 = {?TUPLE_FORM, [VX, VY, L2]},
    EA1 = {?ABSTRACTION_FORM, {[], L2}},
    EA2 = {?ABSTRACTION_FORM, {[VX], VY}},
    EAP1 = {?APPLICATION_FORM, {VX, [VY]}},
    ELT1 = {?LET_FORM, {VX, L2, VY}},
    ELR1 = {?LETREC_FORM, {[{VX, EA1}], VX}},
    ECL1 = {?CLAUSE_FORM, {VY, L3, L2}},
    ECS1 = {?CASE_FORM, {VX, [ECL1]}},
    ERC1 = {?RECEIVE_FORM, {[ECL1], L2, L3}},
    io:format("L1: ~p~n~n", [analyze:get_constraints(L1)]),
    io:format("L2: ~p~n~n", [analyze:get_constraints(L2)]),
    io:format("L3: ~p~n~n", [analyze:get_constraints(L3)]),
    io:format("VX: ~p~n~n", [analyze:get_constraints(VX)]),
    io:format("VY: ~p~n~n", [analyze:get_constraints(VY)]),
    io:format("EL1: ~p~n~n", [analyze:get_constraints(EL1)]),
    io:format("EL2: ~p~n~n", [analyze:get_constraints(EL2)]),
    io:format("ET1: ~p~n~n", [analyze:get_constraints(ET1)]),
    io:format("EA1: ~p~n~n", [analyze:get_constraints(EA1)]),
    io:format("EA2: ~p~n~n", [analyze:get_constraints(EA2)]),
    io:format("EAP1: ~p~n~n", [analyze:get_constraints(EAP1)]),
    io:format("ELT1: ~p~n~n", [analyze:get_constraints(ELT1)]),
    io:format("ELR1: ~p~n~n", [analyze:get_constraints(ELR1)]),
    io:format("ECS1: ~p~n~n", [analyze:get_constraints(ECS1)]),
    io:format("ERC1: ~p~n~n", [analyze:get_constraints(ERC1)]),
    io:format("[=== End Test ===]~n").

%%===========================================================================================
