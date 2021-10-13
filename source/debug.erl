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
-module(debug).
-author("Gorka Suárez García").
-export([
    execute/1, execute/2, block/2, block/3, environment/3, environment/4,
    print/1, print/2, print/3
]).

%%===========================================================================================
%% Apply Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies a function when the debug flag is true.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Victim) ->
    execute(Victim, normal).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies a function when the debug flag is true.
%% @end
%%-------------------------------------------------------------------------------------------
execute(Victim, full) ->
    case data:get_show_debug() andalso data:get_full_debug() of
        true -> Victim();
        _ -> ok
    end;
execute(Victim, _) ->
    case data:get_show_debug() of
        true -> Victim();
        _ -> ok
    end.

%%===========================================================================================
%% Block Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Prints a block of values with some format.
%%
%% EXAMPLE: debug:block("name", [{"value", Value}])
%% @end
%%-------------------------------------------------------------------------------------------
block(Name, Victims) ->
    block(Name, Victims, normal).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Prints a block of values with some format.
%% @end
%%-------------------------------------------------------------------------------------------
block(Name, Victims, Extra) ->
    execute(make_on_execute(Name, Victims), Extra).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun block/2' and 'fun block/3'.
%% @end
%%-------------------------------------------------------------------------------------------
make_on_execute(Name, Victims) ->
    fun() ->
        io:format("****************************************~n"),
        io:format("BLOCK: ~s~n~n", [Name]),
        print(Victims),
        io:format("****************************************~n~n")
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Prints a block of values with some format.
%% @end
%%-------------------------------------------------------------------------------------------
environment(Name, Victims, Environment) ->
    environment(Name, Victims, Environment, normal).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Prints a block of values with some format.
%% @end
%%-------------------------------------------------------------------------------------------
environment(Name, Victims, Environments, Extra) when is_list(Environments) ->
    NextVictims = lists:flatten([environment_to_list(N, E) || {N, E} <- Environments]),
    block(Name, util:list_append(Victims, NextVictims), Extra);
environment(Name, Victims, Environment, Extra) ->
    block(Name, util:list_append(Victims, environment_to_list(Environment)), Extra).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Converts an environment into a list of strings to print.
%% @end
%%-------------------------------------------------------------------------------------------
environment_to_list(Name, Environment) ->
    [{section, Name} | environment_to_list(Environment)].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Converts an environment into a list of strings to print.
%% @end
%%-------------------------------------------------------------------------------------------
environment_to_list(Environment) ->
    [{N, text:type(T), s1} || {N, T} <- maps:to_list(Environment)].

%%===========================================================================================
%% Print Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Prints some values with some format.
%% @end
%%-------------------------------------------------------------------------------------------
print([separator | Values]) ->
    io:format("----------------------------------------~n~n"),
    print(Values);
print([{section, Name} | Values]) ->
    io:format("----------------------------------------~n"),
    io:format("SECTION: ~s~n~n", [Name]),
    print(Values);
print([{Name, Value} | Values]) ->
    print(Name, Value),
    print(Values);
print([{Name, Value, Format} | Values]) ->
    print(Name, Value, Format),
    print(Values);
print([_ | Values]) ->
    print(Values);
print(_) ->
    ok.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Prints a value with some format.
%% @end
%%-------------------------------------------------------------------------------------------
print(Name, Value) ->
    print(Name, Value, p).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Prints a value with some format.
%% @end
%%-------------------------------------------------------------------------------------------
print(Name, Value, Format) ->
    io:format(get_format(Format), [Name, Value]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Prints a value with some format.
%% @end
%%-------------------------------------------------------------------------------------------
get_format(Format) ->
    case Format of
        s1 -> "DEBUG(~s): ~s~n~n";  % Oneline string output.
        w1 -> "DEBUG(~s): ~w~n~n";  % Oneline raw output.
        p1 -> "DEBUG(~s): ~p~n~n";  % Oneline pretty output.
        s  -> "DEBUG(~s):~n~s~n~n"; % String output.
        w  -> "DEBUG(~s):~n~w~n~n"; % Raw output.
        _  -> "DEBUG(~s):~n~p~n~n"  % Pretty output.
    end.
