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
-module(data).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([
    % gen_server callbacks
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,

    % API: General
    start_link/0, reset/0, stop/0,

    % API: Identifiers & Type Variables
    reset_sequence/0, current_name/0, current_variable/0, next_name/0, next_variable/0,
    get_name/1, get_variable/1, reset_identifier/0, get_identifier/0, fresh_variable/0,
    get_rename_table/1,

    % API: Source Code
    get_code/0, set_code/1,

    % API: Types Table
    get_type/2, set_type/3, exists_type/2, remove_type/2,

    % API: Tool Settings
    get_setting/1, get_setting/2, set_setting/2,
    get_rec_max_depth/0, set_rec_max_depth/1,
    get_is_related/0, set_is_related/1,
    get_full_substitution/0, set_full_substitution/1,
    get_condition_relax/0, set_condition_relax/1,
    get_experimental/0, set_experimental/1,
    get_show_code/0, set_show_code/1,
    get_show_linked/0, set_show_linked/1,
    get_show_debug/0, set_show_debug/1,
    get_full_debug/0, set_full_debug/1
]).
-include("type.hrl").
-include("data.hrl").

%%===========================================================================================
%% Macros
%%===========================================================================================

-define(SERVER, ?MODULE).

-record(state, {
    % The sequence number to generate names:
    number = 0,

    % The identifier to make fresh variable names:
    identifier = 0,

    % The source code being analyzed by the tool:
    code = [],

    % The functional types environment library:
    types = #{},

    % The table of flags inside the server:
    settings = #{
        ?REC_MAX_DEPTH          => ?DEFAULT_DEPTH,
        ?FLAG_IS_RELATED        => false,
        ?FLAG_FULL_SUBSTITUTION => false,
        ?FLAG_CONDITION_RELAX   => false,
        ?FLAG_EXPERIMENTAL      => false,
        ?FLAG_SHOW_CODE         => false,
        ?FLAG_SHOW_LINKED       => false,
        ?FLAG_SHOW_DEBUG        => false,
        ?FLAG_FULL_DEBUG        => false
    }
}).

%%===========================================================================================
%% API: General
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Starts the server.
%% @returns {ok, Pid :: pid()} | ignore | {error, Reason :: term()}
%% @end
%%-------------------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Resets the server.
%% @end
%%-------------------------------------------------------------------------------------------
reset() ->
    gen_server:cast(?SERVER, reset).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Stops the server.
%% @end
%%-------------------------------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, exterminate).

%%===========================================================================================
%% API: Identifiers & Type Variables
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Resets the current sequence of names inside the server.
%% @end
%%-------------------------------------------------------------------------------------------
reset_sequence() ->
    gen_server:cast(?SERVER, reset_sequence).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the current name inside the server.
%% @returns The current name value.
%% @end
%%-------------------------------------------------------------------------------------------
current_name() ->
    gen_server:call(?SERVER, current_name).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the current variable inside the server.
%% @returns The current variable value.
%% @end
%%-------------------------------------------------------------------------------------------
current_variable() ->
    type:variable(current_name()).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the next name inside the server.
%% @returns The next name value.
%% @end
%%-------------------------------------------------------------------------------------------
next_name() ->
    gen_server:call(?SERVER, next_name).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the next variable inside the server.
%% @returns The next variable value.
%% @end
%%-------------------------------------------------------------------------------------------
next_variable() ->
    type:variable(next_name()).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a name inside the server.
%% @param Names The list of names to avoid.
%% @returns The name value.
%% @end
%%-------------------------------------------------------------------------------------------
get_name(Names) ->
    Name = current_name(),
    case lists:member(Name, Names) of
        false ->
            Name;
        true ->
            next_name(),
            get_name(Names)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a variable inside the server.
%% @param Names The list of names to avoid.
%% @returns The variable value.
%% @end
%%-------------------------------------------------------------------------------------------
get_variable(Names) ->
    type:variable(get_name(Names)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Resets the current sequence of identifier names inside the server.
%% @end
%%-------------------------------------------------------------------------------------------
reset_identifier() ->
    gen_server:cast(?SERVER, reset_identifier).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the current identifier inside the server.
%% @returns The current identifier value.
%% @end
%%-------------------------------------------------------------------------------------------
get_identifier() ->
    gen_server:call(?SERVER, get_identifier).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a fresh type variable.
%% @returns The new type variable.
%% @end
%%-------------------------------------------------------------------------------------------
fresh_variable() ->
    type:variable(get_identifier()).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a rename table with new identifier names.
%% @param Names The list of names to rename.
%% @returns The new rename table.
%% @end
%%-------------------------------------------------------------------------------------------
get_rename_table(Victims) when is_list(Victims) ->
    maps:from_list([{Key, get_identifier()} || Key <- Victims]);
get_rename_table(_) ->
    maps:new().

%%===========================================================================================
%% API: Source Code
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the current code to analyze inside the server.
%% @returns The current code inside the server.
%% @end
%%-------------------------------------------------------------------------------------------
get_code() ->
    gen_server:call(?SERVER, get_code).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the current code to analyze inside the server.
%% @param Code The code to set inside the server.
%% @end
%%-------------------------------------------------------------------------------------------
set_code(Code) when is_list(Code) ->
    gen_server:cast(?SERVER, {set_code, Code}).

%%===========================================================================================
%% API: Types Table
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the type for a registered function.
%% @param Name The name used to register the function.
%% @param Size The number of parameters of the function.
%% @returns The type of the function if exists; otherwise returns 'none()'.
%% @end
%%-------------------------------------------------------------------------------------------
get_type(Name, Size) ->
    gen_server:call(?SERVER, {get_type, {Name, Size}}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the type for a function.
%% @param Name The name to register the function.
%% @param Size The number of parameters of the function.
%% @param Type The type of the function.
%% @end
%%-------------------------------------------------------------------------------------------
set_type(Name, Size, Type) ->
    gen_server:cast(?SERVER, {set_type, {Name, Size}, Type}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a function exists in the table of types.
%% @param Name The name used to register the function.
%% @param Size The number of parameters of the function.
%% @returns Returns 'true' if the type is registered; otherwise returns 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
exists_type(Name, Size) ->
    gen_server:cast(?SERVER, {exists_type, {Name, Size}}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes the type for a registered function.
%% @param Name The name used to register the function.
%% @param Size The number of parameters of the function.
%% @end
%%-------------------------------------------------------------------------------------------
remove_type(Name, Size) ->
    gen_server:cast(?SERVER, {remove_type, {Name, Size}}).

%%===========================================================================================
%% API: Tool Settings
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a setting inside the server.
%% @param Name The name of the setting.
%% @returns The value of the setting.
%% @end
%%-------------------------------------------------------------------------------------------
get_setting(Name) ->
    gen_server:call(?SERVER, {get_setting, Name}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a setting inside the server.
%% @param Name The name of the setting.
%% @param Default The default value to return.
%% @returns The value of the setting.
%% @end
%%-------------------------------------------------------------------------------------------
get_setting(Name, Default) ->
    gen_server:call(?SERVER, {get_setting, Name, Default}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets a setting inside the server.
%% @param Name The name of the setting.
%% @param Value The value of the setting.
%% @end
%%-------------------------------------------------------------------------------------------
set_setting(Name, Value) ->
    gen_server:cast(?SERVER, {set_setting, Name, Value}).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the recursive maximum depth.
%% @returns The value of the setting.
%% @end
%%-------------------------------------------------------------------------------------------
get_rec_max_depth() -> get_setting(?REC_MAX_DEPTH).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the recursive maximum depth.
%% @param Value The value of the setting.
%% @end
%%-------------------------------------------------------------------------------------------
set_rec_max_depth(Value) ->
    case Value >= ?MIN_DEPTH andalso Value =< ?MAX_DEPTH of
        true -> set_setting(?REC_MAX_DEPTH, Value);
        _ -> input_out_of_range
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the is related flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_is_related() -> get_setting(?FLAG_IS_RELATED).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the is related flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_is_related(Value) -> set_setting(?FLAG_IS_RELATED, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the full substitution flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_full_substitution() -> get_setting(?FLAG_FULL_SUBSTITUTION).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the full substitution flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_full_substitution(Value) -> set_setting(?FLAG_FULL_SUBSTITUTION, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the experimental flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_condition_relax() -> get_setting(?FLAG_CONDITION_RELAX).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the experimental flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_condition_relax(Value) -> set_setting(?FLAG_CONDITION_RELAX, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the experimental flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_experimental() -> get_setting(?FLAG_EXPERIMENTAL).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the experimental flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_experimental(Value) -> set_setting(?FLAG_EXPERIMENTAL, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the show code flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_show_code() -> get_setting(?FLAG_SHOW_CODE).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the show code flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_show_code(Value) -> set_setting(?FLAG_SHOW_CODE, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the show linked flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_show_linked() -> get_setting(?FLAG_SHOW_LINKED).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the show linked flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_show_linked(Value) -> set_setting(?FLAG_SHOW_LINKED, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the show debug flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_show_debug() -> get_setting(?FLAG_SHOW_DEBUG).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the show debug flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_show_debug(Value) -> set_setting(?FLAG_SHOW_DEBUG, Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the full debug flag.
%% @returns The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
get_full_debug() -> get_setting(?FLAG_FULL_DEBUG).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets the full debug flag.
%% @param Value The value of the flag.
%% @end
%%-------------------------------------------------------------------------------------------
set_full_debug(Value) -> set_setting(?FLAG_FULL_DEBUG, Value).

%%===========================================================================================
%% gen_server callbacks
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%% @param Args term()
%% @returns {ok, State :: #state{}}
%%        | {ok, State :: #state{}, timeout() | hibernate}
%%        | {stop, Reason :: term()}
%%        | ignore
%% @end
%%-------------------------------------------------------------------------------------------
init([]) -> {ok, #state{}}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages.
%% @param Request term()
%% @param From {pid(), Tag :: term()}
%% @param State #state{}
%% @returns {reply, Reply :: term(), NewState :: #state{}}
%%        | {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate}
%%        | {noreply, NewState :: #state{}}
%%        | {noreply, NewState :: #state{}, timeout() | hibernate}
%%        | {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}
%%        | {stop, Reason :: term(), NewState :: #state{}}
%% @end
%%-------------------------------------------------------------------------------------------
handle_call(current_name, _, State) ->
    {reply, util:make_atom(State#state.number), State};

handle_call(next_name, _, State) ->
    Victim = State#state.number + 1,
    {reply, util:make_atom(Victim), State#state{ number = Victim }};

handle_call(get_identifier, _, State) ->
    Victim = State#state.identifier,
    {reply, Victim, State#state{ identifier = Victim + 1 }};

handle_call(get_code, _, State) ->
    {reply, State#state.code, State};

handle_call({get_type, Key = {Name, Size}}, _, State) ->
    case language:is_bif(Name) of
        true -> {reply, language:get_bif_type(Name, Size), State};
        false -> {reply, util:map_get(State#state.types, Key, type:none()), State}
    end;

handle_call({exists_type, Key = {Name, _}}, _, State) ->
    case language:is_bif(Name) of
        true -> {reply, true, State};
        false -> {reply, util:map_has_key(State#state.types, Key), State}
    end;

handle_call({get_setting, Name}, _, State) ->
    {reply, util:map_get(State#state.settings, Name, false), State};

handle_call({get_setting, Name, Default}, _, State) ->
    {reply, util:map_get(State#state.settings, Name, Default), State};

handle_call(_, _, State) ->
    {reply, ok, State}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages.
%% @param Request term()
%% @param State #state{}
%% @returns {noreply, NewState :: #state{}}
%%        | {noreply, NewState :: #state{}, timeout() | hibernate}
%%        | {stop, Reason :: term(), NewState :: #state{}}
%% @end
%%-------------------------------------------------------------------------------------------
handle_cast(reset, _) ->
    {noreply, #state{}};

handle_cast(exterminate, State) ->
    {stop, {exterminate, annihilate, destroy}, State};

handle_cast(reset_sequence, State) ->
    {noreply, State#state{ number = 0 }};

handle_cast(reset_identifier, State) ->
    {noreply, State#state{ identifier = 0 }};

handle_cast({set_code, Code}, State) ->
    {noreply, State#state{ code = Code }};

handle_cast({set_type, Key = {Name, _}, Type}, State) ->
    case language:is_bif(Name) of
        true -> {noreply, State};
        false -> {noreply, State#state{ types = util:map_set(State#state.types, Key, Type) }}
    end;

handle_cast({remove_type, Key}, State) ->
    {noreply, State#state{ types = util:map_remove(State#state.types, Key) }};

handle_cast({set_setting, Name, Value}, State) ->
    {noreply, State#state{ settings = util:map_set(State#state.settings, Name, Value) }};

handle_cast(_, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages.
%% @param Info timeout() | term()
%% @param State #state{}
%% @returns {noreply, NewState :: #state{}}
%%        | {noreply, NewState :: #state{}, timeout() | hibernate}
%%        | {stop, Reason :: term(), NewState :: #state{}}
%% @end
%%-------------------------------------------------------------------------------------------
handle_info(_, State) -> {noreply, State}.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to terminate. It should be
%% the opposite of Module:init/1 and do any necessary cleaning up. When it returns, the
%% gen_server terminates with Reason. The return value is ignored.
%% @param Reason (normal | shutdown | {shutdown, term()} | term())
%% @param State #state{}
%% @returns term()
%% @end
%%-------------------------------------------------------------------------------------------
terminate(_, _) -> ok.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed.
%% @param OldVsn term() | {down, term()}
%% @param State #state{}
%% @param Extra term()
%% @returns {ok, NewState :: #state{}}
%%        | {error, Reason :: term()}
%% @end
%%-------------------------------------------------------------------------------------------
code_change(_, State, _) -> {ok, State}.
