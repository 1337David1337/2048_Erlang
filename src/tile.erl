% modified by Alexander Tang

-module(tile).

-export([tilemain/1, tilemain/2, tilemain/3]).

tilemain( Id ) ->
	tilemain(Id, 0).

tilemain( Id, Value ) ->
	tilemain(Id, Value, false).

tilemain( Id, Value, SendConfirm) ->
	glob:registerName(list_to_atom(integer_to_list(Id)), self()),
	case SendConfirm of
		true -> manager ! confirmready;
		false -> 
			tilelife(Id, Value, false),
			ok
	end,
	tilelife(Id, Value, false).

tilelife(Id, CurrentValue, Merged)->
	receive
		die ->
			debug:debug("I, ~p, die.~n",[Id]),
			exit({killed, Id, CurrentValue});
		up ->
			tileoperation(Id, fun upf/1, fun upstop/1, fun dnf/1, fun dnstop/1, CurrentValue, up);
		dn ->
			tileoperation(Id, fun dnf/1, fun dnstop/1, fun upf/1, fun upstop/1, CurrentValue, dn);
		lx ->
			tileoperation(Id, fun lxf/1, fun lxstop/1, fun rxf/1, fun rxstop/1, CurrentValue, lx);
		rx ->
			tileoperation(Id, fun rxf/1, fun rxstop/1, fun lxf/1, fun lxstop/1, CurrentValue, rx);
		{yourValue, Repl} ->
			Repl ! {tilevalue, Id, CurrentValue, Merged};
		{setvalue, Future, NewMerged} ->
			tilelife(Id, Future, NewMerged),
			ok
	end,
	tilelife(Id, CurrentValue, false).

upf(X) -> X - 4.
dnf(X) -> X + 4.
lxf(X) -> X - 1.
rxf(X) -> X + 1.

upstop(X) -> X < 1.
dnstop(X) -> X > 16.
lxstop(X) -> (X rem 4) == 0.
rxstop(X) -> (X rem 4) == 1.

% main body of the tile directions
tileoperation(Id, F, FStop, G, GStop, Value, Command) ->
	T = futuretilelist(F(Id), F, FStop, []),
	processtile(lists:reverse(T), Value, false, 0),
	NextTile = G(Id),
	case GStop(NextTile) of
		true ->
			manager ! ready,
			ok;
		false ->
			glob:regformat(NextTile) ! Command
	end.

% retrieves all possible future tiles in a list
futuretilelist(StartValue, F, Cond, TileList) ->
	case Cond(StartValue) of
		true ->
			TileList;
		false ->
			futuretilelist(F(StartValue), F, Cond, [StartValue | TileList])
	end.

% checks where the tile needs to move to
processtile([], _, _, 0) -> ok;
processtile([], Value, Merged, NewPosition) ->
	glob:regformat(NewPosition) ! {setvalue, Value, Merged},
	self() ! {setvalue, 0, false},
	ok;
processtile([H|R], Value, Merged, NewPosition) ->
	try
		glob:regformat(H) ! {yourValue, self()}
	catch
		_:_ ->
			timer:sleep(10),
			processtile([H|R], Value, Merged, NewPosition),
			ok
	after
		{OtherValue, OtherMerged} = receivetileinfo(),
		case OtherValue == 0 of
			true ->
				processtile(R, Value, Merged, H);
			false ->
				case (OtherValue == Value andalso Merged == false andalso OtherMerged == false) of
					true ->
						processtile(R, double(Value), true, H);
					false ->
						processtile([], Value, Merged, NewPosition)
				end
		end
	end.

double(X) -> 2*X.
	
% receives the info from possible future tiles
receivetileinfo() ->
	receive
		{tilevalue, _, CurrentValue, Merged} ->
			{CurrentValue, Merged}
	end.
