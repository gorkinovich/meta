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
%% Variable Status Classes
%%===========================================================================================

-define(VARIABLE_SINGLE, single ).
-define(VARIABLE_PLURAL, plural ).

%%===========================================================================================
%% Variable Location Classes
%%===========================================================================================

-define(LOCATION_NORMAL,         normal         ).
-define(LOCATION_SINGLETON,      singleton      ).
-define(LOCATION_CONSTRAINT,     constraint     ).
-define(LOCATION_SUBSETEQ_LEFT,  subseteq_left  ).
-define(LOCATION_SUBSETEQ_RIGHT, subseteq_right ).
-define(LOCATION_MATCH,          match          ).

%%===========================================================================================
%% Variable Info Structure
%%===========================================================================================

-record(varinfo, {
    name,
    count     = 1,
    status    = ?VARIABLE_SINGLE,
    location  = ?LOCATION_NORMAL,
    relatives = []
}).
