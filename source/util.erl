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
-module(util).
-author("Gorka Suárez García").
-export([
    % General Functions:
    id/1, substitution/2,

    % Atom Functions:
    atom_append/2, atom_append/3, make_atom/1,

    % List Functions:
    list_append/2, list_intersection/2, list_filter/3, list_product/1, list_product/2,
    list_any_member/2, list_all_member/2, list_map/2, list_flatten/1, list_usort/1,
    list_usort_flatten/1, list_usort_append/2, list_flatten_map/2, list_find/4,
    list_sum/1, list_check/2, list_max/1,

    % Flags Functions:
    flags_all/1, flags_any/1,

    % String Functions:
    is_string/1, is_list_of_strings/1, is_white_space/1, is_empty_line/1,
    remove_empty_lines/1, remove_text_empty_lines/1, to_string/1, to_string/3,
    join_strings/2,

    % Map Functions:
    map_get/3, map_set/3, map_update/4, map_remove/2, map_init/3,
    map_has_key/2, map_set_keys/3, map_remove_keys/2, map_init_keys/3,
    map_equal_key/3, map_equal_keys/3, map_check_key/4, map_check_keys/4,
    map_join/3, map_join/2,

    % File Functions:
    load_text_file/1, load_tuples_file/1,

    % Parser Functions:
    get_text_file_tokens/2, get_string_tokens/2
]).

%%===========================================================================================
%% Macros
%%===========================================================================================

-define(ALPHABET_SIZE, 26).
-define(FIRST_LETTER,  65).

%%===========================================================================================
%% General Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% The identity function.
%% @param Value The value to return.
%% @returns The same given value.
%% @end
%%-------------------------------------------------------------------------------------------
id(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Substitutes a value with another one.
%% @param Victim The value to change.
%% @param Table The translation table.
%% @returns The changed value or the same.
%% @end
%%-------------------------------------------------------------------------------------------
substitution(Victim, {Victim, NextValue}) ->
    NextValue;
substitution(Victim, Table) when is_map(Table) ->
    map_get(Table, Victim, Victim);
substitution(Victim, _) ->
    Victim.

%%===========================================================================================
%% Atom Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Appends some value to an atom.
%% @param Victim The value to modify.
%% @param Value The value to append.
%% @returns The new atom value.
%% @end
%%-------------------------------------------------------------------------------------------
atom_append(Victim, Value) when is_integer(Value) ->
    list_to_atom(atom_to_list(Victim) ++ integer_to_list(Value));
atom_append(Victim, Value) when is_float(Value) ->
    list_to_atom(atom_to_list(Victim) ++ float_to_list(Value));
atom_append(Victim, Value) when is_atom(Value) ->
    list_to_atom(atom_to_list(Victim) ++ atom_to_list(Value));
atom_append(Victim, Value) when is_list(Value) ->
    list_to_atom(atom_to_list(Victim) ++ Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Appends some value to an atom.
%% @param Victim The value to modify.
%% @param Separator The separator value to append.
%% @param Value The value to append.
%% @returns The new atom value.
%% @end
%%-------------------------------------------------------------------------------------------
atom_append(Victim, Separator, Value) ->
    atom_append(atom_append(Victim, Separator), Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Makes an atom from an integer number.
%% @param Number The value to transform.
%% @returns The new atom value.
%% @end
%%-------------------------------------------------------------------------------------------
make_atom(Number) when is_integer(Number) ->
    list_to_atom(make_atom(abs(Number), []));
make_atom(_) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Utility function for 'fun make_atom/1'.
%% @end
%%-------------------------------------------------------------------------------------------
make_atom(Number, String) ->
    Letter = (Number rem ?ALPHABET_SIZE) + ?FIRST_LETTER,
    NextString = [Letter | String],
    case Number < ?ALPHABET_SIZE of
        true ->
            NextString;
        false ->
            NextNumber = (Number div ?ALPHABET_SIZE) - 1,
            make_atom(NextNumber, NextString)
    end.

%%===========================================================================================
%% List Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Appends two lists or values into one single list.
%% @param Left The left value in the operation.
%% @param Right The right value in the operation.
%% @returns The new list with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_append(Left, Right) when is_list(Left), is_list(Right) ->
    lists:append(Left, Right);
list_append(Left, Right) when is_list(Left) ->
    lists:append(Left, [Right]);
list_append(Left, Right) when is_list(Right) ->
    lists:append([Left], Right);
list_append(Left, Right) ->
    [Left, Right].

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the intersection between two lists of values.
%% @param Left The left value in the operation.
%% @param Right The right value in the operation.
%% @returns The new list with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_intersection(Left, Right) when is_list(Left), is_list(Right) ->
    lists:filter(fun(Value) -> lists:member(Value, Right) end, Left).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Filters a list with a selected list of elements.
%% @param Victim The list to be filtered.
%% @param Elements The selected elements.
%% @param Flag The type of filter.
%% @returns The new filtered list.
%% @end
%%-------------------------------------------------------------------------------------------
list_filter(Victim, Elements, 'notin') when is_list(Victim), is_list(Elements) ->
    lists:filter(fun(Value) -> not lists:member(Value, Elements) end, Victim);
list_filter(Victim, Elements, 'in') when is_list(Victim), is_list(Elements) ->
    lists:filter(fun(Value) -> lists:member(Value, Elements) end, Victim).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Creates a list of lists after apply the cartesian product.
%% @param Lists The list of lists to apply the operation.
%% @returns The new list of lists with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_product([]) ->
    [[]];
list_product(Values) when is_list(Values) ->
    NextValues = normalize_list_product_input(Values),
    case NextValues of
        [L] ->
            [L];
        [L | LS] ->
            lists:reverse(list_product(true, L, LS, [], []))
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Creates a list of lists after apply the cartesian product.
%% @param X_XS The current list to iterate.
%% @param YS_YSS The pending list of lists to iterate.
%% @param ACS The accumulated elements from each list.
%% @param RS The accumulated results of the cartesian product.
%% @returns The new list of lists with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_product(false, [], _, _, RS) ->
    RS;
list_product(true, [], [], ACS, RS) ->
    [lists:reverse(ACS) | RS];
list_product(true, [], [YS | YSS], ACS, RS) ->
    list_product(true, YS, YSS, ACS, RS);
list_product(_, [X | XS], [], ACS, RS) ->
    NACS = lists:reverse([X | ACS]),
    list_product(false, XS, [], ACS, [NACS | RS]);
list_product(_, [X | XS], [YS | YSS], ACS, RS) ->
    NRS = list_product(true, YS, YSS, [X | ACS], RS),
    list_product(false, XS, [YS | YSS], ACS, NRS).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Creates a list of tuples after apply the cartesian product.
%% @param Left The left list to apply the operation.
%% @param Right The right list to apply the operation.
%% @returns The new list with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_product(Left, Right) when is_list(Left), is_list(Right) ->
    [{L, R} || L <- Left, R <- Right];
list_product(Left, Right) when is_list(Left) ->
    [{L, Right} || L <- Left];
list_product(Left, Right) when is_list(Right) ->
    [{Left, R} || R <- Right];
list_product(Left, Right) ->
    [{Left, Right}].

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes a list into a list of lists.
%% @param Types The list to normalize.
%% @returns The normalized list of lists.
%% @end
%%-------------------------------------------------------------------------------------------
normalize_list_product_input([]) ->
    [];
normalize_list_product_input([Victim | Tail]) when is_list(Victim) ->
    [Victim | normalize_list_product_input(Tail)];
normalize_list_product_input([Victim | Tail]) ->
    [[Victim] | normalize_list_product_input(Tail)].

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if any member inside the candidates are inside the values.
%% @param Candidates The elements to check inside the list of values.
%% @param Values The list of values to be checked.
%% @returns The true if any member of the candidates are inside the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_any_member(Candidates, Values) ->
    lists:any(fun id/1, [lists:member(Victim, Values) || Victim <- Candidates]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if all the members inside the candidates are inside the values.
%% @param Candidates The elements to check inside the list of values.
%% @param Values The list of values to be checked.
%% @returns The true if all the members in the candidates are inside the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_all_member(Candidates, Values) ->
    lists:all(fun id/1, [lists:member(Victim, Values) || Victim <- Candidates]).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps a function over a list.
%% @param Value The value to map.
%% @param Func The function to apply.
%% @returns The new mapped list.
%% @end
%%-------------------------------------------------------------------------------------------
list_map(Values, Func) when is_list(Values) ->
    [Func(Value) || Value <- Values];
list_map(Value, Func) ->
    Func(Value).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Flattens a list of values.
%% @param Value The list of change.
%% @returns The changed list.
%% @end
%%-------------------------------------------------------------------------------------------
list_flatten(Value) when is_list(Value) ->
    lists:flatten(Value);
list_flatten(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Unique-sorts a list of values.
%% @param Value The list of change.
%% @returns The changed list.
%% @end
%%-------------------------------------------------------------------------------------------
list_usort(Value) when is_list(Value) ->
    lists:usort(Value);
list_usort(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Unique-sorts and flattens a list of values.
%% @param Value The list of change.
%% @returns The changed list.
%% @end
%%-------------------------------------------------------------------------------------------
list_usort_flatten(Values) when is_list(Values) ->
    lists:usort(lists:flatten(Values));
list_usort_flatten(Value) ->
    Value.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Appends two lists or values into one single list and applies an unique-sort.
%% @param Left The left value in the operation.
%% @param Right The right value in the operation.
%% @returns The new list with all the values.
%% @end
%%-------------------------------------------------------------------------------------------
list_usort_append(Left, Right) ->
    lists:usort(list_append(Left, Right)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Maps a function over a flatten list.
%% @param Value The value to map.
%% @param Func The function to apply.
%% @returns The new mapped flatten list.
%% @end
%%-------------------------------------------------------------------------------------------
list_flatten_map(Values, Func) when is_list(Values) ->
    list_flatten([list_flatten_map(Value, Func) || Value <- lists:flatten(Values)]);
list_flatten_map(Value, Func) ->
    list_flatten(Func(Value)).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Finds a value inside a list.
%% @param Values The list to check.
%% @param Default The default value to return.
%% @param OnCheck The function that checks if the value is found.
%% @param OnReturn The function that returns the final value.
%% @returns The found value or the default one.
%% @end
%%-------------------------------------------------------------------------------------------
list_find([], Default, _, _) ->
    Default;
list_find([Value | Values], Default, OnCheck, OnReturn) ->
    case OnCheck(Value) of
        true -> OnReturn(Value);
        _ -> list_find(Values, Default, OnCheck, OnReturn)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sums a list of numbers.
%% @param Value The list to sum.
%% @returns The final sum.
%% @end
%%-------------------------------------------------------------------------------------------
list_sum(Values) when is_list(Values) ->
    lists:sum(lists:flatten(Values));
list_sum(Value) when is_integer(Value) ->
    Value;
list_sum(_) ->
    0.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a list fulfills a predicate.
%% @param Value The list to check.
%% @param Predicate The predicate to apply.
%% @returns Returns 'true' if the list passes the check, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
list_check([], _) ->
    true;
list_check([Value | Values], Predicate) ->
    case Predicate(Value) of
        true -> list_check(Values, Predicate);
        _ -> false
    end;
list_check(_, _) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the maximum value inside a list.
%% @param Value The list to check.
%% @returns The maximum value or zero.
%% @end
%%-------------------------------------------------------------------------------------------
list_max([]) ->
    0;
list_max(Victim) when is_list(Victim) ->
    lists:max(Victim);
list_max(Victim) when is_integer(Victim) ->
    Victim;
list_max(_) ->
    0.

%%===========================================================================================
%% Flags Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if all the flags in a list are true.
%% @param Value The list to check.
%% @returns Returns 'true' if all the flags are true.
%% @end
%%-------------------------------------------------------------------------------------------
flags_all(Victim) when is_list(Victim) ->
    lists:all(fun util:id/1, Victim);
flags_all(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if any of the flags in a list are true.
%% @param Value The list to check.
%% @returns Returns 'true' if any of the flags are true.
%% @end
%%-------------------------------------------------------------------------------------------
flags_any(Victim) when is_list(Victim) ->
    lists:any(fun util:id/1, Victim);
flags_any(_) ->
    false.

%%===========================================================================================
%% String Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a list is a string value or not.
%% @param Value The list value to check.
%% @returns Returns 'true' if the value is a string, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_string(Value) ->
    list_check(Value, fun(Victim) -> is_integer(Victim) end).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a list is a list of strings value or not.
%% @param Value The list value to check.
%% @returns Returns 'true' if the value is a list of strings, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_list_of_strings(Value) ->
    list_check(Value, fun(Victim) -> is_string(Victim) end).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if an ASCII value is a white space character (space, new line, tab).
%% @param Value The integer ASCII character value to check.
%% @returns Returns 'true' if the value is a white space, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_white_space(Value) when is_integer(Value) ->
    (Value =:= $\n) orelse (Value =:= $\r) orelse (Value =:= $\t) orelse (Value =:= $ );
is_white_space(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a line is empty (a line that only contains white space characters).
%% @param Value The text line value to check.
%% @returns Returns 'true' if the value is an empty line, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
is_empty_line(Value) when is_list(Value) ->
    lists:all(fun is_white_space/1, Value);
is_empty_line(_) ->
    false.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes the empty lines inside a text.
%% @param Values The text value to filter.
%% @returns The text without empty lines.
%% @end
%%-------------------------------------------------------------------------------------------
remove_empty_lines(Values) when is_list(Values) ->
    [V || V <- Values, not is_empty_line(V)];
remove_empty_lines(Values) ->
    Values.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes the empty lines from a source code text.
%% @param Text The source code text value to filter.
%% @returns The source code text without the empty lines.
%% @end
%%-------------------------------------------------------------------------------------------
remove_text_empty_lines(Text) ->
    LS1 = string:tokens(Text, "\n"),
    LS2 = remove_empty_lines(LS1),
    string:join(LS2, "\n").

%%-------------------------------------------------------------------------------------------
%% @doc
%% Converts basic values into a string.
%% @param Value The value to convert.
%% @returns The value turn into a string.
%% @end
%%-------------------------------------------------------------------------------------------
to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(Value) when is_integer(Value) -> integer_to_list(Value);
to_string(Value) when is_float(Value) -> float_to_list(Value);
to_string(Value) when is_list(Value) -> Value;
to_string(_) -> "".

%%-------------------------------------------------------------------------------------------
%% @doc
%% Converts list into a string.
%% @param ToString The function to apply to the elements on the list.
%% @param Separator The separator string between elements.
%% @param Value The list to convert.
%% @returns The list turn into a string.
%% @end
%%-------------------------------------------------------------------------------------------
to_string(_, _, []) ->
    "";
to_string(ToString, _, [Value | []]) ->
    ToString(Value);
to_string(ToString, Separator, [Value | Values]) ->
    ToString(Value) ++ Separator ++ to_string(ToString, Separator, Values).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Joins a list of strings.
%% @param Separator The separator string between elements.
%% @param Value The list of strings.
%% @returns The list turn into a string.
%% @end
%%-------------------------------------------------------------------------------------------
join_strings(_, []) ->
    "";
join_strings(_, [Value | []]) ->
    Value;
join_strings(Separator, [Value | Values]) ->
    Value ++ Separator ++ join_strings(Separator, Values).

%%===========================================================================================
%% Map Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets a value inside a map.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @param Default The default value to return if the key is not found.
%% @returns The value related to the key if found, otherwise the default value.
%% @end
%%-------------------------------------------------------------------------------------------
map_get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
map_get(_, _, Default) ->
    Default.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets a value inside a map.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @param Value The value to set.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_set(Map, Key, Value) when is_map(Map) ->
    maps:put(Key, Value, Map);
map_set(Map, _, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Updates a value inside a map.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @param Default The default value of the key to find.
%% @param OnValue The function to apply over the current value.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_update(Map, Key, Default, OnValue) when is_map(Map) ->
    Current = map_get(Map, Key, Default),
    map_set(Map, Key, OnValue(Current));
map_update(Map, _, _, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes a value inside a map.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_remove(Map, Key) when is_map(Map) ->
    maps:remove(Key, Map);
map_remove(Map, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets a value inside a map only when doesn't exist.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @param Value The value to set.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_init(Map, Key, Value) when is_map(Map) ->
    case maps:is_key(Key, Map) of
        false -> maps:put(Key, Value, Map);
        _ -> Map
    end;
map_init(Map, _, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if a map has a key.
%% @param Map The map with the data.
%% @param Key The key to find inside the map.
%% @returns 'true' if the key exists; otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
map_has_key(Map, Key) when is_map(Map) ->
    maps:is_key(Key, Map);
map_has_key(Map, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets some values inside a map.
%% @param Map The map with the data.
%% @param Keys The keys to find inside the map.
%% @param Value The value to set.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_set_keys(Map, Keys, Value) when is_list(Keys) ->
    lists:foldl(fun(Key, Accum) -> map_set(Accum, Key, Value) end, Map, Keys);
map_set_keys(Map, _, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Removes some values inside a map.
%% @param Map The map with the data.
%% @param Keys The keys to find inside the map.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_remove_keys(Map, Keys) when is_list(Keys) ->
    lists:foldl(fun(Key, Accum) -> map_remove(Accum, Key) end, Map, Keys);
map_remove_keys(Map, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Sets some values inside a map only when doesn't exist.
%% @param Map The map with the data.
%% @param Keys The keys to find inside the map.
%% @param Value The value to set.
%% @returns The new changed map.
%% @end
%%-------------------------------------------------------------------------------------------
map_init_keys(Map, Keys, Value) when is_list(Keys) ->
    lists:foldl(fun(Key, Accum) -> map_init(Accum, Key, Value) end, Map, Keys);
map_init_keys(Map, _, _) ->
    Map.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if the value of a key is equal in two tables.
%% @param Key The key to check inside the map.
%% @param Left The left map with the data.
%% @param Right The right map with the data.
%% @returns 'true' when the values are the same, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
map_equal_key(Key, Left, Right) ->
    LV = map_get(Left, Key, null),
    RV = map_get(Right, Key, null),
    LV =:= RV.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks if some keys are equal in two tables.
%% @param Keys The keys to check inside the map.
%% @param Left The left map with the data.
%% @param Right The right map with the data.
%% @returns 'true' when the values are the same, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
map_equal_keys([], _, _) ->
    true;
map_equal_keys([Key | Keys], Left, Right) ->
    case map_equal_key(Key, Left, Right) of
        true -> map_equal_keys(Keys, Left, Right);
        _ -> false
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks a property with the values of a key in two tables.
%% @param Key The key to check inside the map.
%% @param Left The left map with the data.
%% @param Right The right map with the data.
%% @param Property The property to check.
%% @returns 'true' if the property is true, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
map_check_key(Key, Left, Right, Property) ->
    LV = map_get(Left, Key, null),
    RV = map_get(Right, Key, null),
    Property(LV, RV).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Checks a property with the values of some keys in two tables.
%% @param Keys The keys to check inside the map.
%% @param Left The left map with the data.
%% @param Right The right map with the data.
%% @param Property The property to check.
%% @returns 'true' if the property is true, otherwise 'false'.
%% @end
%%-------------------------------------------------------------------------------------------
map_check_keys([], _, _, _) ->
    true;
map_check_keys([Key | Keys], Left, Right, Property) ->
    case map_check_key(Key, Left, Right, Property) of
        true -> map_check_keys(Keys, Left, Right, Property);
        _ -> false
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Joins two maps into one map.
%% @param Left The left map.
%% @param Right The right map.
%% @param Func The join function.
%% @returns The joined map.
%% @end
%%-------------------------------------------------------------------------------------------
map_join(Left, Right, Func) when is_map(Left), is_map(Right) ->
    lists:foldl(
        fun(Key, Table) ->
            case maps:is_key(key, Left) of
                true ->
                    LeftValue = maps:get(Key, Left),
                    RightValue = maps:get(Key, Right),
                    maps:put(Key, Func(LeftValue, RightValue), Table);
                _ ->
                    maps:put(Key, maps:get(Key, Right), Table)
            end
        end,
        Left,
        maps:keys(Right)
    );
map_join(_, _, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @doc
%% Joins a list of maps into one map.
%% @param Values The list of maps.
%% @param Func The join function.
%% @returns The joined map.
%% @end
%%-------------------------------------------------------------------------------------------
map_join(Values, Func) when is_list(Values) ->
    lists:foldl(
        fun(Value, Table) ->
            map_join(Value, Table, Func)
        end,
        maps:new(),
        lists:flatten(Values)
    );
map_join(Value, _) ->
    Value.

%%===========================================================================================
%% File Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Loads a text file.
%% @param Filename The path of the file to load.
%% @returns The loaded text into a string.
%% @end
%%-------------------------------------------------------------------------------------------
load_text_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> binary_to_list(Binary);
        {error, Reason} -> {error, Reason}
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Loads a file of Erlang tuples.
%% @param Filename The path of the file to load.
%% @returns The loaded tuples.
%% @end
%%-------------------------------------------------------------------------------------------
load_tuples_file(Filename) ->
    case file:consult(Filename) of
        {ok, Terms} -> Terms;
        {error, _} -> []
    end.

%%===========================================================================================
%% Parser Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the tokens inside a text file.
%% @param Filename The path of the file to load.
%% @param Keywords A callback function that identifies the keywords.
%% @returns The loaded tokens from the file.
%% @end
%%-------------------------------------------------------------------------------------------
get_text_file_tokens(Filename, Keywords) ->
    case load_text_file(Filename) of
        {error, _} -> [];
        String -> get_string_tokens(String, Keywords)
    end.

%%-------------------------------------------------------------------------------------------
%% @doc
%% Gets the tokens inside a string.
%% @param String The string with the tokens to get.
%% @param Keywords A callback function that identifies the keywords.
%% @returns The loaded tokens from the string.
%% @end
%%-------------------------------------------------------------------------------------------
get_string_tokens(String, Keywords) ->
    case erl_scan:string(String, 1, [{reserved_word_fun, Keywords}]) of
        {ok, Tokens, _} -> Tokens;
        {error, _, _} -> []
    end.
