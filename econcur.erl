-module(econcur).
-export([parse_transform/2]).
-define(PARPID, '__ParentProc').
-define(RESPID, '__exec_result').

-spec parse_transform(_, _) -> _.
parse_transform(AST, _) ->
	run_trans(AST, fun match_trans/1).

commands([], _, _) -> [];
commands(All = [H|R], Init, UnInit) ->
	case [ X || X <- UnInit, references(H, X) ] of
		[Var|_] ->
			% The following command contains a reference to an uninitialised variable
			% Receive the variable, then reassess
			LineNum = element(2, H),
			[
				{'receive', LineNum,
					[
						{clause, LineNum,
							[
								{tuple, LineNum, 
									[
										{atom, LineNum, ?RESPID},
										{atom, LineNum, Var},
										{var, LineNum, Var}
									]
								}
							],
							[],
							commands(All, Init ++ [Var], UnInit -- [Var])
						}
					]
				}
			];
		[] ->
			case H of
				{match, MatchLN, {var, _, VarName}, RHS} ->
					% An assignment is being made
					% Execute the command in parralell and add the var to uninit list
					[
						{
							block,
							MatchLN,
							[
								{
									match,
									MatchLN,
						    		{var, MatchLN, ?PARPID},
									{call, MatchLN, {atom, MatchLN, self}, []}
						   		},
								{call, MatchLN,
									{atom, MatchLN, spawn},
									[
										{'fun', MatchLN,
											{clauses,
												[
													{clause, MatchLN, [], [],
								 						[
															{op, MatchLN, '!',
																{var, MatchLN, ?PARPID},
																{tuple, MatchLN,
																	[
																		{
																			atom,
																			MatchLN,
																			?RESPID
																		},
																		{
																			atom,
																			MatchLN,
																			VarName
																		},
									 									RHS
																	]
																}
															}
														]
													}
												]
											}
										}
									]
								}
							]
						}
					| commands(R, Init, UnInit ++ [VarName]) ];
				_ -> [ H | commands(R, Init, UnInit) ]
			end
	end.

clauses(Cls) ->
	lists:map(
		fun({clause, NumLine, Vars, Other, Cmds}) ->
			{clause,
				NumLine,
				Vars,
				Other,
				commands(
					Cmds,
					[ Var || {var, _, Var} <- Vars, Var =/= '_' ],
					[]
				)
			}
		end,
		Cls
	).

%% Tree traversal and matching framework

references(C, VarName) ->
	% If running the following transformation returns the same as the input
	% the variable is not referenced
	run_trans(
		C,
		fun(XVarName) ->
			if VarName == XVarName -> var_found;
			true -> ignore_trans
			end
		end
	) =/= C.

match_trans({function, NumLine, FunName, NumArgs, Clauses}) ->
	show_form({function, NumLine, FunName, NumArgs, clauses(Clauses)});
match_trans({block, NumLine, Cmds}) ->
	show({block, NumLine, run_trans(commands(Cmds, [], []), fun match_trans/1)});
match_trans(_) -> ignore_trans.

run_trans(AST, Fun) ->
	case Fun(AST) of
		ignore_trans -> trans(AST, Fun);
		NewAST -> NewAST
	end.

trans(AST, Fun) when is_tuple(AST) -> list_to_tuple(trans(tuple_to_list(AST), Fun));
trans(AST, Fun) when is_list(AST) -> [ run_trans(SubT, Fun) || SubT <- AST ];
trans(Else, _) -> Else.

% DEBUGGING HELPERS

show_form(F) ->
	io:format("~s~n", [lists:flatten(erl_pp:form(F))]),
	F.

show(X) ->
	io:format("~p~n", [X]),
	X.
