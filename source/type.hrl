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
-author("Gorka Suárez García").

%%===========================================================================================
%% Type Classes
%%===========================================================================================

-define(TYPE_VARIABLE,    'VAR'    ). % {Class, Name, Linked}
-define(TYPE_LITERAL,     'LIT'    ). % {Class, Value}
-define(TYPE_SET,         'SET'    ). % {Class, Name, Parameters}
-define(TYPE_SCHEME,      'SCHEME' ). % {Class, Variables, Inner}
-define(TYPE_LAMBDA,      'LAMBDA' ). % {Class, Parameters, Result}
-define(TYPE_CONDITION,   'WHEN'   ). % {Class, Type, Constraints}
-define(TYPE_UNION,       'UNION'  ). % {Class, Ordered, Types}
-define(TYPE_SYMBOL_CALL, '@CALL'  ). % {Class, Name, Parameters}
-define(TYPE_PAIR,        'PAIR'   ). % {Class, Type, Constraints}

%%===========================================================================================
%% Type Sets (Basic)
%%===========================================================================================

-define(TYPE_ANY,     'any'     ).
-define(TYPE_NONE,    'none'    ).
-define(TYPE_ATOM,    'atom'    ).
-define(TYPE_INTEGER, 'integer' ).
-define(TYPE_FLOAT,   'float'   ).
-define(TYPE_TUPLE,   'tuple'   ).
-define(TYPE_NELIST,  'nelist'  ).

%%===========================================================================================
%% Type Sets (Extended Basic)
%%===========================================================================================

-define(TYPE_FUN,  'fun'       ).
-define(TYPE_MAP,  'map'       ).
-define(TYPE_BIN,  'binary'    ).
-define(TYPE_PID,  'pid'       ).
-define(TYPE_PORT, 'port'      ).
-define(TYPE_REF,  'reference' ).

%%===========================================================================================
%% Type Sets (Extended Numeric)
%%===========================================================================================

-define(TYPE_NEGINT, 'negint'  ). % NEG_INF..-1
-define(TYPE_NAT,    'natural' ). % 0..INF
-define(TYPE_NZNAT,  'nznat'   ). % 1..INF
-define(TYPE_ARITY,  'arity'   ). % 0..255
-define(TYPE_BYTE,   'byte'    ). % 0..255
-define(TYPE_CHAR,   'char'    ). % 0..16#10FFFF

%%===========================================================================================
%% Type Sets (Basic Alias)
%%===========================================================================================

-define(TYPE_NORETURN, 'noreturn' ). % noreturn() = none()
-define(TYPE_BOOLEAN,  'boolean'  ). % boolean()  = 'true' + 'false'
-define(TYPE_NUMBER,   'number'   ). % number()   = integer() + float()
-define(TYPE_NIL,      'nil'      ). % nil()      = []
-define(TYPE_LIST,     'list'     ). % list()     = [] + nelist(any(), [])
-define(TYPE_STRING,   'string'   ). % string()   = [] + nelist(char(), [])
-define(TYPE_NESTRING, 'nestring' ). % nestring() = nelist(char(), [])
-define(TYPE_TIMEOUT,  'timeout'  ). % timeout()  = 'infinity' + natural()

%%===========================================================================================
%% Type Sets (Extended Alias)
%%===========================================================================================

-define(TYPE_NODE,       'node'       ). % node()       = atom()
-define(TYPE_MODULE,     'module'     ). % module()     = atom()
-define(TYPE_MFA,        'mfa'        ). % mfa()        = {module(), atom(), arity()}
-define(TYPE_IDENTIFIER, 'identifier' ). % identifier() = pid() + port() + ref()

%%===========================================================================================
%% Constraint Classes
%%===========================================================================================

-define(CONSTRAINTS_BOTTOM, bottom).

-define(CONSTRAINT_MATCH,    'MATCH'    ). % {Class, LType, RType} = LType := RType
-define(CONSTRAINT_SUBSETEQ, 'SUBSETEQ' ). % {Class, LType, RType} = LType <= RType
-define(CONSTRAINT_JOINABLE, 'JOINABLE' ). % {Class, Variables}    = <Variables>

%%===========================================================================================
%% Erlang Types Specifications Differences
%%===========================================================================================

% |-----------------------------------|----------------------------------------|
% | Erlang Types Specifications       | META Types Specifications              |
% |-----------------------------------|----------------------------------------|
% | no_return()                       | noreturn()                             |
% | neg_integer()                     | negint()                               |
% | non_neg_integer()                 | natural()                              |
% | pos_integer()                     | nznat()                                |
% | nonempty_list()                   | nelist()                               |
% | nonempty_string()                 | nestring()                             |
% |-----------------------------------|----------------------------------------|
% | nonempty_list()                   | nelist(any(),[])                       |
% | nonempty_list(H)                  | nelist(H,[])                           |
% | nonempty_improper_list(H,T)       | nelist(H,T)                            |
% | maybe_improper_list()             | [] | nelist(any(),any())               |
% | maybe_improper_list(H,T)          | [] | nelist(H,[]) + nelist(H,T)        |
% | nonempty_maybe_improper_list()    | nelist(any(),[]) + nelist(any(),any()) |
% | nonempty_maybe_improper_list(H,T) | nelist(H,[]) + nelist(H,T)             |
% |-----------------------------------|----------------------------------------|

% Erlang Reference Manual:
% http://erlang.org/doc/reference_manual/typespec.html
