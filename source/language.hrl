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
%% Constant Values
%%===========================================================================================

-define(TRUE,     'true'     ).
-define(FALSE,    'false'    ).
-define(INFINITY, 'infinity' ).

%%===========================================================================================
%% Form Classes
%%===========================================================================================

-define(LITERAL_FORM,     'LIT' ).
-define(VARIABLE_FORM,    'VAR' ).
-define(LIST_FORM,        'LST' ).
-define(TUPLE_FORM,       'TPL' ).
-define(ABSTRACTION_FORM, 'ABS' ).
-define(APPLICATION_FORM, 'APP' ).
-define(LET_FORM,         'LET' ).
-define(LETREC_FORM,      'LRC' ).
-define(CASE_FORM,        'CAS' ).
-define(RECEIVE_FORM,     'RCV' ).
-define(CLAUSE_FORM,      'CLS' ).
-define(FUNNAME_FORM,     'FN'  ).
