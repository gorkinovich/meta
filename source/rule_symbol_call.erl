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
-module(rule_symbol_call).
-author("Gorka Suárez García").
-export([execute/3]).
-include("type.hrl").
-include("reduce.hrl").

%%===========================================================================================
%% Call Functions
%%===========================================================================================

%%-------------------------------------------------------------------------------------------
%% @doc
%% Applies the rule to transform constraints with a symbolic call.
%% @returns {Table, Status}
%% @end
%%-------------------------------------------------------------------------------------------
execute(_, ?CONSTRAINTS_BOTTOM, Status) ->
    {?CONSTRAINTS_BOTTOM, Status};
execute(Name, Table, Status) ->
    Constraints = table:get_definition_constraints(Table, Name),
    util:list_flatten_map(
        execute(Constraints, [], [], Status),
        fun({NewCons, OldCons}) ->
            NextTable = table:set_definition_constraints(Table, Name, OldCons),
            {FinalTable, DirtyNames} = table:split_constraints(NextTable, NewCons),
            {FinalTable, tools:update_status_dirty(Status, Name, DirtyNames)}
        end
    ).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Applies the rule to transform constraints with a symbolic call.
%% @returns {NewCons, OldCons}
%% @end
%%-------------------------------------------------------------------------------------------
execute([], NewCons, OldCons, _) ->
    {lists:usort(NewCons), lists:usort(OldCons)};
execute([Constraint | Constraints], NewCons, OldCons, Status) ->
    case Constraint of
        {?CONSTRAINT_MATCH, LeftType = {?TYPE_VARIABLE, _, _},
            {?TYPE_SYMBOL_CALL, {?TYPE_VARIABLE, CallName, CallLinked}, CallParams}} ->
            % When the constraint is a symbol call, the first step is to get the
            % current functional type for the call, and check if the type is none:
            CallSize = length(CallParams),
            FuncType = tools:get_call_type(Status, CallName, CallSize),
            case tools:is_func_none(FuncType) of
                true ->
                    % When the type is none, we'll return the bottom constraints:
                    {?CONSTRAINTS_BOTTOM, ?CONSTRAINTS_BOTTOM};

                false ->
                    % Whe the type isn't none, we'll check if it's the any type:
                    case tools:is_func_any(FuncType) of
                        true ->
                            % When the type is any or the any lambda type, we'll create
                            % a generic lambda type with fresh type variables, then we'll
                            % create the new call constraints:
                            GFT = create_generic_lambda(CallSize),
                            CLCS = make_call_constraints(GFT, LeftType, CallParams),
                            % Then we merge the created constraints with the accumulated
                            % value of the current step and continue with the loop:
                            CLCS2 = constraint:insert_match(CLCS, type:variable(CallName, CallLinked), GFT),
                            NNCS = constraint:merge(NewCons, CLCS2),
                            execute(Constraints, NNCS, OldCons, Status);

                        false ->
                            % For each functional type inside a sequence we'll create the
                            % new call constraints and continue with the loop:
                            Continue = fun(FT) ->
                                CLCS = make_call_constraints(FT, LeftType, CallParams),
                                NNCS = constraint:merge(NewCons, CLCS),
                                execute(Constraints, NNCS, OldCons, Status)
                                       end,
                            case FuncType of
                                {?TYPE_UNION, true, Types} ->
                                    [Continue(Type) || Type <- Types];
                                _ ->
                                    Continue(FuncType)
                            end
                    end
            end;
        _ ->
            % When the constraint isn't a symbol call, we move it to the old constraints
            % that will be returned in the end of the loop over the constraints:
            execute(Constraints, NewCons, [Constraint | OldCons], Status)
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes the call constraints for a functional type.
%% @end
%%-------------------------------------------------------------------------------------------
make_call_constraints({?TYPE_SCHEME, Variables, InnerType}, CallResult, CallParams) ->
    make_call_constraints(Variables, InnerType, CallResult, CallParams);
make_call_constraints({?TYPE_LAMBDA, Parameters, ResultType}, CallResult, CallParams) ->
    make_call_constraints([], type:lambda(Parameters, ResultType), CallResult, CallParams);
make_call_constraints(_, _, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes the call constraints for a functional type.
%% @end
%%-------------------------------------------------------------------------------------------
make_call_constraints(ClosedVars, {?TYPE_LAMBDA, Params, ResultType}, CallResult, CallParams) ->
    % We get the linked and the instantiable variables from the lambda type:
    LLKVS = query:get_linked_variables(type:lambda(Params, ResultType)),
    LITVS = query:get_instantiable_variables(type:lambda(Params, ResultType)),
    % Then a substitution map is made for the closed and instantiable variables:
    RenMap = data:get_rename_table(ClosedVars ++ LITVS),
    % Here we get the list of open instantiable variables in the lambda:
    OITVS = LITVS -- ClosedVars,
    % Then we will create the constaints for the call of the functional type:
    FCS1 = constraint:insert_match(constraint:conjunction(), CallResult, type:rename(ResultType, RenMap)),
    FCS2 = constraint:merge(FCS1, [make_call_param_constraint(CP, type:rename(PR, RenMap)) || {CP, PR} <- lists:zip(CallParams, Params)]),
    constraint:merge(FCS2, [tools:make_subseteq_constraint(LLKVS, maps:get(Beta, RenMap), Beta) || Beta <- OITVS]);
make_call_constraints(_, _, _, _) ->
    throw(not_supported).

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Makes a call parameter constraint with a couple of types.
%% @end
%%-------------------------------------------------------------------------------------------
make_call_param_constraint(Left, Right) ->
    case type:is_variable(Left) of
        true ->
            constraint:match(Left, Right);
        false ->
            case type:is_variable(Right) of
                true -> constraint:match(Right, Left);
                _ -> constraint:subseteq(Left, Right)
            end
    end.

%%-------------------------------------------------------------------------------------------
%% @private
%% @doc
%% Creates a generic lambda with type variables.
%% @end
%%-------------------------------------------------------------------------------------------
create_generic_lambda(Size) ->
    type:lambda([data:fresh_variable() || _ <- lists:seq(1, Size)], data:fresh_variable()).
