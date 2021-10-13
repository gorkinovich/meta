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
-module(meta).
-author("Gorka Suárez García").
-export([main/0, main/1, make_parser/0]).
-include("data.hrl").

%%===========================================================================================
%% Macros
%%===========================================================================================

-define(EXIT_CODE, exit).
-define(HELP_CODE, help).
-define(VERSION_CODE, version).
-define(SHOW_RUN_TEST, true).

%%===========================================================================================
%% Entry Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% The main entry function of the command.
%% @end
%%-------------------------------------------------------------------------------------------
main() ->
    case init:get_plain_arguments() of
        [] -> show_help();
        Args -> main(Args)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% The main entry function of the command.
%% @param Args The input arguments of the command.
%% @end
%%-------------------------------------------------------------------------------------------
main(Args) when is_list(Args) ->
    loop_args(initialize(Args));
main(_) ->
    show_help().

%%-------------------------------------------------------------------------------------------
%% @doc
%% Creates the erlang module to parse the Mini-Erlang language.
%% @end
%%-------------------------------------------------------------------------------------------
make_parser() ->
    yecc:file("minierlang.yrl").

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the help of the command.
%% @end
%%-------------------------------------------------------------------------------------------
show_help() ->
    io:format("META: Mini-Erlang Typing Application~n~n"),
    io:format("COMMAND [options] files~n~n"),
    io:format("Options:~n"),
    io:format(" -h, --help          Shows this help message.~n"),
    io:format(" -v, --version       Shows the version message.~n"),
    io:format(" -d, --debug         Shows the debug messages.~n"),
    io:format(" -f, --fulldebug     Shows the debug full messages.~n"),
    io:format(" -c, --code          Shows the source code of the files.~n"),
    io:format(" -x, --experimental  Uses some experimental algorithms.~n"),
    io:format("~n~n").

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the version of the command.
%% @end
%%-------------------------------------------------------------------------------------------
show_version() ->
    io:format("META: Mini-Erlang Typing Application~n"),
    io:format("Version: alfa 0.1~n").

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the command.
%% @param Args The input arguments of the command.
%% @returns The list of files.
%% @end
%%-------------------------------------------------------------------------------------------
initialize(Args) ->
    data:start_link(),
    {Options, Files} = get_options_and_files(Args),
    case set_options(Options) of
        success -> Files;
        _ -> ride_on_time
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Loops over the input arguments of the command.
%% @param Args The input arguments of the command.
%% @end
%%-------------------------------------------------------------------------------------------
loop_args([]) ->
    ok;
loop_args([F|FS]) when is_list(F) ->
    check_and_analyze_file(F),
    loop_args(FS);
loop_args(F) when is_list(F) ->
    check_and_analyze_file(F);
loop_args(_) ->
    ok.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a file exist and analyzes the source code to get the types inside.
%% @param Filename The path of the file to load.
%% @end
%%-------------------------------------------------------------------------------------------
check_and_analyze_file(Filename) ->
    case filelib:is_regular(Filename) of
        true -> analyze_file(Filename);
        false -> io:format("[ERROR] File ~s doesn't exists!~n", [Filename])
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Analyzes a source file to get the types inside.
%% @param Filename The path of the file to load.
%% @end
%%-------------------------------------------------------------------------------------------
analyze_file(Filename) ->
    show_begin(Filename),
    show_code(Filename),
    case language:parse(Filename) of
        {error, Reason} ->
            io:format("[ERROR] ~p~n", [Reason]);
        Forms ->
            data:reset_identifier(),
            data:set_code(Forms),
            lists:map(
                fun(Pair) ->
                    show_initial_pair(Pair),
                    NewPair = reduce:execute(Pair),
                    show_final_pair(NewPair)
                end,
                analyze:get_constraints(Forms)
            )
    end,
    show_end().

%%===========================================================================================
%% Show Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the beginning of a file analysis.
%% @param Filename The path of the file.
%% @end
%%-------------------------------------------------------------------------------------------
show_begin(Filename) ->
    io:format("[=========== Begin Analysis ===========]~n"),
    io:format("FILE: ~s~n", [Filename]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the code of a file analysis.
%% @param Filename The path of the file.
%% @end
%%-------------------------------------------------------------------------------------------
show_code(Filename) ->
    Code = util:load_text_file(Filename),
    case data:get_show_code() of
        true ->
            io:format("[-----------  Source Code   -----------]~n"),
            io:format("~s~n", [Code]);
        _ ->
            ok
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the initial pair of the analysis.
%% @param Pair The pair to show.
%% @end
%%-------------------------------------------------------------------------------------------
show_initial_pair(Pair) ->
    case data:get_show_debug() of
        true ->
            io:format("[-----------  Constraints   -----------]~n"),
            io:format("TYPE:~n~s~n~n", [text:type(Pair)]);
        _ ->
            ok
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the final pair of the analysis.
%% @param Pair The pair to show.
%% @end
%%-------------------------------------------------------------------------------------------
show_final_pair(Pair) ->
    case data:get_show_debug() orelse data:get_show_code() of
        true ->
            io:format("[----------- Transformation -----------]~n");
        _ ->
            ok
    end,
    io:format("TYPE:~n~s~n~n", [text:type(Pair)]).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Shows the ending of a file analysis.
%% @end
%%-------------------------------------------------------------------------------------------
show_end() ->
    io:format("[===========  End Analysis  ===========]~n~n").

%%===========================================================================================
%% Options Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options and filenames from an input.
%% @param Victim The input values to work with.
%% @returns The options and the files to analyze.
%% @end
%%-------------------------------------------------------------------------------------------
get_options_and_files(Victim) ->
    case util:is_string(Victim) of
        true ->
            get_options_or_file(Victim);
        _ ->
            case util:is_list_of_strings(Victim) of
                true ->
                    get_options_and_files(Victim, [], []);
                _ ->
                    {cylons_detected, gaius_baltar}
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options and filenames from an input.
%% @param Victim The input values to work with.
%% @param Options The output values for the options.
%% @param Filenames The output values for the filenames.
%% @returns The options and the files to analyze.
%% @end
%%-------------------------------------------------------------------------------------------
get_options_and_files([], Options, Filenames) ->
    {lists:usort(Options), lists:reverse(Filenames)};
get_options_and_files([Victim | Victims], Options, Filenames) ->
    {NewOptions, NewFilename} = get_options_or_file(Victim),
    case NewOptions of
        ?EXIT_CODE ->
            {?EXIT_CODE, []};
        _ ->
            NextOptions = util:list_append(NewOptions, Options),
            NextFilenames = case NewFilename of
                                [] -> Filenames;
                                _ -> [NewFilename | Filenames]
                            end,
            get_options_and_files(Victims, NextOptions, NextFilenames)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options or a filename from an input.
%% @param Victim The input values to work with.
%% @returns The options or the file to analyze.
%% @end
%%-------------------------------------------------------------------------------------------
get_options_or_file([]) ->
    {[], []};
get_options_or_file(Victim) ->
    case hd(Victim) of
        $- -> {get_options(Victim), []};
        _ -> {[], Victim}
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options from an input.
%% @param Victim The input values to work with.
%% @returns The options founded in the input.
%% @end
%%-------------------------------------------------------------------------------------------
get_options(Victim) ->
    case Victim of
        "-" ->
            on_option_error();
        "-h" ->
            on_option_help();
        "-v" ->
            on_option_version();
        "--help" ->
            on_option_help();
        "--version" ->
            on_option_version();
        "--runtest" ->
            on_run_test();
        "--fulldebug" ->
            [?FLAG_SHOW_DEBUG, ?FLAG_FULL_DEBUG];
        "--debug" ->
            ?FLAG_SHOW_DEBUG;
        "--code" ->
            ?FLAG_SHOW_CODE;
        "--experimental" ->
            ?FLAG_EXPERIMENTAL;
        _ ->
            case get_inner_options(tl(Victim), []) of
                ?EXIT_CODE ->
                    on_option_error();
                ?HELP_CODE ->
                    on_option_help();
                ?VERSION_CODE ->
                    on_option_version();
                Result ->
                    Result
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options from an input.
%% @param Victim The input values to work with.
%% @returns The options founded in the input.
%% @end
%%-------------------------------------------------------------------------------------------
get_inner_options([], Result) ->
    Result;
get_inner_options([$h | _], _) ->
    ?HELP_CODE;
get_inner_options([$v | _], _) ->
    ?VERSION_CODE;
get_inner_options([$f | Victims], Result) ->
    get_inner_options(Victims, [?FLAG_SHOW_DEBUG, ?FLAG_FULL_DEBUG | Result]);
get_inner_options([$d | Victims], Result) ->
    get_inner_options(Victims, [?FLAG_SHOW_DEBUG | Result]);
get_inner_options([$c | Victims], Result) ->
    get_inner_options(Victims, [?FLAG_SHOW_CODE | Result]);
get_inner_options([$x | Victims], Result) ->
    get_inner_options(Victims, [?FLAG_EXPERIMENTAL | Result]);
get_inner_options([$0 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 0} | Result]);
get_inner_options([$1 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 1} | Result]);
get_inner_options([$2 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 2} | Result]);
get_inner_options([$3 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 3} | Result]);
get_inner_options([$4 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 4} | Result]);
get_inner_options([$5 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 5} | Result]);
get_inner_options([$6 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 6} | Result]);
get_inner_options([$7 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 7} | Result]);
get_inner_options([$8 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 8} | Result]);
get_inner_options([$9 | Victims], Result) ->
    get_inner_options(Victims, [{?REC_MAX_DEPTH, 9} | Result]);
get_inner_options(_, _) ->
    ?EXIT_CODE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Gets the options and filenames from an input.
%% @param Victim The input values to work with.
%% @returns Returns 'engage' when everything is ok.
%% @end
%%-------------------------------------------------------------------------------------------
set_options(Options) ->
    set_options(Options, null).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun set_options/1'.
%% @end
%%-------------------------------------------------------------------------------------------
set_options([], null) ->
    success;
set_options([], Depth) ->
    data:set_rec_max_depth(Depth),
    success;
set_options([Option | Options], Extra) ->
    case Option of
        ?EXIT_CODE ->
            ?EXIT_CODE;
        {?REC_MAX_DEPTH, Depth} ->
            set_options(Options, select_depth(Extra, Depth));
        _ ->
            data:set_setting(Option, true),
            set_options(Options, Extra)
    end;
set_options(_, _) ->
    ?EXIT_CODE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun set_options/2'.
%% @end
%%-------------------------------------------------------------------------------------------
select_depth(null, Right) when is_integer(Right) ->
    Right;
select_depth(Left, Right) when is_integer(Left), is_integer(Right) ->
    min(Left, Right);
select_depth(_, _) ->
    null.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The on error event for the command.
%% @returns The exit code for the command.
%% @end
%%-------------------------------------------------------------------------------------------
on_option_error() ->
    io:format("[ERROR] Bad command options!!!~n~n"),
    show_help(),
    ?EXIT_CODE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The on help event for the command.
%% @returns The exit code for the command.
%% @end
%%-------------------------------------------------------------------------------------------
on_option_help() ->
    show_help(),
    ?EXIT_CODE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The on version event for the command.
%% @returns The exit code for the command.
%% @end
%%-------------------------------------------------------------------------------------------
on_option_version() ->
    show_version(),
    ?EXIT_CODE.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% The on run test event for the command.
%% @returns The exit code for the command.
%% @end
%%-------------------------------------------------------------------------------------------
on_run_test() ->
    show_version(),
    io:format("~n"),
    data:set_show_debug(true),
    data:set_full_debug(true),
    case ?SHOW_RUN_TEST of
        true -> tests:execute();
        _ -> io:format("Hello, world!~n")
    end,
    ?EXIT_CODE.
