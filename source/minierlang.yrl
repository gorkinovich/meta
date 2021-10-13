%%********************************************************************************
%% MIT License
%%
%% Copyright (c) 2019 Gorka Suárez García
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%********************************************************************************

Nonterminals
    rlit rvar rpat rpats rguard rclause rclauses rparams rfun rldef rldefs
    rarg rargs rapp ruplus ruminus rexp rexps rforms.

Terminals
    atom float integer var char string '==' '/=' '=<' '<' '>=' '>' '=:=' '=/='
    '++' '--' '+' '-' '*' '/' 'bnot' 'div' 'rem' 'band' 'bor' 'bxor' 'bsl' 'bsr'
    'not' 'and' 'or' 'xor' 'orelse' 'andalso' ',' '[' '|' ']' '{' '}' '(' ')'
    '->' 'fun' 'let' 'in' 'letrec' 'case' 'of' 'end' 'receive' 'after' 'when'
    '=' ':'.

Rootsymbol rforms.

%%--------------------------------------------------------------------------------

%Nonassoc   10 'catch'.
%Right      20 '=' '!'.
Left        30 'orelse'.
Left        40 'andalso'.
Left        50 '==' '/=' '=<' '<' '>=' '>' '=:=' '=/='.
Right       60 '++' '--'.
Left        70 '+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'.
Left        80 '*' '/' 'div' 'rem' 'band' 'and'.
Unary       90 ruplus ruminus 'bnot' 'not'.
%Nonassoc  100 '#'.
%Nonassoc  110 ':'.

%%--------------------------------------------------------------------------------

rlit -> '{' '}' : {'LIT', {}}.
rlit -> '[' ']' : {'LIT', []}.
rlit -> integer : {'LIT', get_value('$1')}.
rlit -> float   : {'LIT', get_value('$1')}.
rlit -> atom    : {'LIT', get_value('$1')}.
rlit -> char    : {'LIT', get_value('$1')}.
rlit -> string  : {'LIT', get_value('$1')}.

rvar -> var : {'VAR', get_value('$1')}.

rarg -> rlit : '$1'.
rarg -> rvar : '$1'.

rargs -> rarg           : ['$1'].
rargs -> rarg ',' rargs : ['$1' | '$3'].

rpat -> rarg                  : '$1'.
rpat -> '[' rpat '|' rpat ']' : {'LST', {'$2', '$4'}}.
rpat -> '[' rpats ']'         : {'LST', '$2'}.
rpat -> '{' rpats '}'         : {'TPL', '$2'}.

rpats -> rpat           : ['$1'].
rpats -> rpat ',' rpats : ['$1' | '$3'].

rguard -> rarg                              : '$1'.
rguard -> rapp                              : '$1'.
rguard -> 'let' rvar '=' rguard 'in' rguard : {'LET', {'$2', '$4', '$6'}}.

rclause -> rpat 'when' rguard '->' rexp : {'CLS', {'$1', '$3', '$5'}}.

rclauses -> rclause          : ['$1'].
rclauses -> rclause rclauses : ['$1' | '$2'].

rparams -> rvar             : ['$1'].
rparams -> rvar ',' rparams : ['$1' | '$3'].

rfun -> 'fun' '(' ')' '->' rexp         : {'ABS', {[], '$5'}}.
rfun -> 'fun' '(' rparams ')' '->' rexp : {'ABS', {'$3', '$6'}}.

rldef -> rvar '=' rfun : {'$1', '$3'}.

rldefs -> rldef        : ['$1'].
rldefs -> rldef rldefs : ['$1' | '$2'].

rapp -> atom '(' rargs ')'          : {'APP', {{'FN', get_value('$1')}, '$3'}}.
rapp -> atom ':' atom '(' rargs ')' : {'APP', {{'FN', get_value('$1'), get_value('$3')}, '$5'}}.
rapp -> rvar '(' rargs ')'          : {'APP', {'$1', '$3'}}.
rapp -> rarg '==' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '/=' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '=<' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '<' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '>=' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '>' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '=:=' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '=/=' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '++' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '--' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '+' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '-' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '*' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg '/' rarg               : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'div' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'rem' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'band' rarg            : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'bor' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'bxor' rarg            : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'bsl' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'bsr' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'and' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'or' rarg              : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'xor' rarg             : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'orelse' rarg          : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> rarg 'andalso' rarg         : {'APP', {get_operator('$2'), ['$1', '$3']}}.
rapp -> 'bnot' rarg                 : {'APP', {get_operator('$1'), ['$2']}}.
rapp -> 'not' rarg                  : {'APP', {get_operator('$1'), ['$2']}}.
rapp -> ruplus                      : '$1'.
rapp -> ruminus                     : '$1'.

ruplus  -> '+' rarg : {'APP', {get_operator('$1'), ['$2']}}.
ruminus -> '-' rarg : {'APP', {get_operator('$1'), ['$2']}}.

rexp -> rarg                                      : '$1'.
rexp -> rfun                                      : '$1'.
rexp -> rapp                                      : '$1'.
rexp -> '[' rexp '|' rexp ']'                     : {'LST', {'$2', '$4'}}.
rexp -> '[' rexps ']'                             : {'LST', '$2'}.
rexp -> '{' rexps '}'                             : {'TPL', '$2'}.
rexp -> 'let' rvar '=' rexp 'in' rexp             : {'LET', {'$2', '$4', '$6'}}.
rexp -> 'letrec' rldefs 'in' rexp                 : {'LRC', {'$2', '$4'}}.
rexp -> 'case' rvar 'of' rclauses 'end'           : {'CAS', {'$2', '$4'}}.
rexp -> 'receive' rclauses 'after' rarg '->' rexp : {'RCV', {'$2', '$4', '$6'}}.

rexps -> rexp           : ['$1'].
rexps -> rexp ',' rexps : ['$1' | '$3'].

rforms -> rexp        : ['$1'].
rforms -> rexp rforms : ['$1' | '$2'].

%%--------------------------------------------------------------------------------

Erlang code.

get_value({_, _, Value}) -> Value.
get_operator({Operator, _}) -> Operator.
