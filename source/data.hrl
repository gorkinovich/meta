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
%% Constant
%%===========================================================================================

-define(DEFAULT_DEPTH, 3).
-define(MIN_DEPTH, 0).
-define(MAX_DEPTH, 9).

%%===========================================================================================
%% Flags
%%===========================================================================================

% Reduce algorithm settings:
-define(REC_MAX_DEPTH, rec_max_depth).

% Reduce algorithm flags:
-define(FLAG_IS_RELATED, is_related).
-define(FLAG_FULL_SUBSTITUTION, full_substitution).
-define(FLAG_CONDITION_RELAX, condition_relax).
-define(FLAG_EXPERIMENTAL, experimental).

% Console output flags:
-define(FLAG_SHOW_CODE, show_code).
-define(FLAG_SHOW_LINKED, show_linked).
-define(FLAG_SHOW_DEBUG, show_debug).
-define(FLAG_FULL_DEBUG, full_debug).
