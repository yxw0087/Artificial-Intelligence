%CMPS 420		Section 1	Project #3
%Wong, Yee H.	yxw0087	    17h April 2015

%Name: Wumpus World Problem Solution Finder
%Problem Statement: This is a program that will attempt to solve a nxn grid Wumpus World Problem.
%Problem Specification: Input to the solver must follow exactly as the test plan format provided in the first line of code. Input must be a solvable problem or the program will enter infinite loop while attempting to solve it.

test(Plan):-
    solve([dimension(5,5),at(agent,[1,1]),facing(agent,east),arrow(2),at(wumpus,[3,3]),status(wumpus,alive),at(gold,[4,2]), at(gold,[4,5]),at(pit,[3,1]),at(pit,[1,5]),status(gold,free)],[at(agent,[1,1]),status(gold,grabbed)],Plan).

solve(State, Goal, Plan):-
	member(dimension(N,N), State), %Get dimension from input
	member(at(wumpus,[Wx,Wy]), State), %Get wumpus postion from input	
	add_stench([Wx,Wy], Stench, N), %Add rooms that can smell stench
	add_breezes(State,[],Breeze,N), %Add rooms that can feel breeze
    solve(State, Goal, [], Plan, [], [], Stench, Breeze, [], []).
	
solve(State, Goal, Plan, Plan, Been, Path, Stench, Breeze, WumpusFree, PitFree):-
        is_subset(Goal, State), nl, append_list([exit(1,1)], Plan, Final),
		write('Gold in your hand, you decided to climb out from [1,1].'), nl, nl,
		write('Summary of actions: '), nl,
        write_sol(Final).
		
solve(State, Goal, Sofar, Plan, Been, Path, Stench, Breeze, WumpusFree, PitFree):-	
		member(dimension(N,N), State),
		member(at(agent,[X,Y]), State), %Get agent's position before action
		member(at(wumpus,[Wx,Wy]), State), %Get wumpus postion from input
		member(status(wumpus,Status), State), %Get the status of the wumpus
		add_adjacent([X,Y], NewAdjacent, N), %Add new adjacent rooms into new adjacent list	
		check_stench(Status, State, State1, [X,Y], [Wx,Wy], Stench, NewAdjacent, WumpusFree, NewWumpusFree, Been),
		check_breeze([X,Y], Breeze, NewAdjacent, PitFree, NewPitFree),
        action([X,Y], Name, NewAdjacent, State1, NewState, Been, NewBeen, Path, NewPath, NewWumpusFree, NewPitFree, N),
        solve(NewState, Goal, [Name|Sofar], Plan, NewBeen, NewPath, Stench, Breeze, NewWumpusFree, NewPitFree).
	
action([X,Y], Name, Adjacent, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N):-
	is_subset([at(gold,[X,Y]), status(gold,free)], State) ->
	grab([X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree);	
	is_subset([status(gold,grabbed)], State) ->
	retreat([X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath);
	priority_direction([X,Y], State, State1, Adjacent, Been, WumpusFree, PitFree),
	member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N).
	
grab([X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree):-
	Name = grab(gold),
	NewBeen = Been, NewPath = Path,
	write('(Gold is glittering in this room! [1x Gold obtained.]'), nl,
	delete_list([status(gold,free)], State, State1),
	append_list([status(gold,grabbed)], State1, NewState).
	
retreat([X,Y], Name, State, NewState, Been, NewBeen, [H|T], NewPath):-
	([X,Y] \= H ->
	write('Retreating...'), nl,
	Name = retreat([X,Y], H),
	NewBeen = Been,
	write(Name),nl,
	delete_list([H], [H|T], NewPath),
	delete_list([at(agent,[X,Y])], State, State1),
	append_list([at(agent,H)], State1, NewState);
	delete_list([H], [H|T], NewPath0), retreat([X,Y], Name, State, NewState, Been, NewBeen, NewPath0, NewPath)).
	
move(Direction, [X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N):-
	Direction == east,
	X2 is X + 1, Y2 is Y, (N >= X2 ->
	(member([X2,Y2], WumpusFree) -> (member([X2,Y2], PitFree) ->
	Name = move([X,Y], [X2,Y2]), write(Name),nl,
	delete_list([at(agent,[X,Y])], State, State1),
	append_list([at(agent,[X2,Y2])], State1, NewState),
	append_list([[X,Y]], Been, NewBeen), append_list([[X,Y]], Path, NewPath0),
	(member([X2,Y2], Been) -> delete_list([[X,Y]], NewPath0, NewPath);NewPath = NewPath0);
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	write('(You bumped into the wall in this direction!)'), nl,
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N)).
	
move(Direction, [X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N):-
	Direction == south,
	X2 is X, Y2 is Y - 1, (Y2 > 0 ->
	(member([X2,Y2], WumpusFree) -> (member([X2,Y2], PitFree) ->
	Name = move([X,Y], [X2,Y2]), write(Name),nl,
	delete_list([at(agent,[X,Y])], State, State1),
	append_list([at(agent,[X2,Y2])], State1, NewState),
	append_list([[X,Y]], Been, NewBeen), append_list([[X,Y]], Path, NewPath0),
	(member([X2,Y2], Been) -> delete_list([[X,Y]], NewPath0, NewPath);NewPath = NewPath0);
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	write('(You bumped into the wall in this direction!)'), nl, 
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N)).
	
move(Direction, [X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N):-
	Direction == west,
	X2 is X - 1, Y2 is Y, (X2 > 0 ->
	(member([X2,Y2], WumpusFree) -> (member([X2,Y2], PitFree) ->
	Name = move([X,Y], [X2,Y2]), write(Name),nl,
	delete_list([at(agent,[X,Y])], State, State1),
	append_list([at(agent,[X2,Y2])], State1, NewState),
	append_list([[X,Y]], Been, NewBeen), append_list([[X,Y]], Path, NewPath0),
	(member([X2,Y2], Been) -> delete_list([[X,Y]], NewPath0, NewPath);NewPath = NewPath0);
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	write('(You bumped into the wall in this direction!)'), nl,
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N)).
	
move(Direction, [X,Y], Name, State, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N):-
	Direction == north,
	X2 is X, Y2 is Y + 1, (N >= Y2 ->
	(member([X2,Y2], WumpusFree) -> (member([X2,Y2], PitFree) ->
	Name = move([X,Y], [X2,Y2]), write(Name),nl,
	delete_list([at(agent,[X,Y])], State, State1),
	append_list([at(agent,[X2,Y2])], State1, NewState),
	append_list([[X,Y]], Been, NewBeen), append_list([[X,Y]], Path, NewPath0),
	(member([X2,Y2], Been) -> delete_list([[X,Y]], NewPath0, NewPath);NewPath = NewPath0);
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N));
	write('(You bumped into the wall in this direction!)'), nl,
	leftturn(Direction, State, State1), member(facing(agent, D), State1),
	move(D, [X,Y], Name, State1, NewState, Been, NewBeen, Path, NewPath, WumpusFree, PitFree, N)).
		
add_stench([Wx,Wy], Stench, N):-
	Wx<N, Wx>1, Wy<N, Wy>1, 
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1, 
	append_list([[Wx3,Wy]], Stench1, Stench2), Wx3 is Wx + 1,
	append_list([[Wx,Wy2]], Stench2, Stench3), Wy2 is Wy - 1,
	append_list([[Wx,Wy3]], Stench3, Stench), Wy3 is Wy + 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=1, Wy=1, 
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx + 1,
	append_list([[Wx,Wy2]], Stench1, Stench), Wy2 is Wy + 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=1, Wy=N, 
	append_list([[Wx,Wy2]], [], Stench1), Wy2 is Wy - 1,
	append_list([[Wx2,Wy]], Stench1, Stench), Wx2 is Wx + 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=N, Wy=1, 
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1,
	append_list([[Wx,Wy2]], Stench1, Stench), Wy2 is Wy + 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=N, Wy=N, 
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1,
	append_list([[Wx,Wy2]], Stench1, Stench), Wy2 is Wy - 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wy=N,
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1,
	append_list([[Wx3,Wy]], Stench1, Stench2), Wx3 is Wx + 1,
	append_list([[Wx,Wy2]], Stench2, Stench), Wy2 is Wy - 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wy=1,
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1,
	append_list([[Wx3,Wy]], Stench1, Stench2), Wx3 is Wx + 1,
	append_list([[Wx,Wy2]], Stench2, Stench), Wy2 is Wy + 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=N,
	append_list([[Wx2,Wy]], [], Stench1), Wx2 is Wx - 1,
	append_list([[Wx,Wy2]], Stench1, Stench2), Wy2 is Wy + 1,
	append_list([[Wx,Wy3]], Stench2, Stench), Wy3 is Wy - 1.
	
add_stench([Wx,Wy], Stench, N):-
	Wx=1,
	append_list([[Wx,Wy2]], [], Stench1), Wy2 is Wy + 1,
	append_list([[Wx2,Wy]], Stench1, Stench2), Wx2 is Wx + 1,
	append_list([[Wx,Wy3]], Stench2, Stench), Wy3 is Wy - 1.
	
%If current room has stench, only add current room to WumpusFree and start thinking about wumpus
check_stench(Status, State, NewState, [X,Y], [Wx,Wy], Stench, Adjacent, WumpusFree, NewWumpusFree, Been):-
	Status == alive,
	member([X,Y],Stench),
	write('(You suddenly smell a horrible stench in this room, a stinky wumpus might be nearby!)'), nl,
	append_list([[X,Y]], WumpusFree, NewWumpusFree0),
	thinkWumpus(State, NewState, [X,Y], [Wx,Wy], Adjacent, Adjacent, NewWumpusFree0, NewWumpusFree, Been, 0).

%Else add current and all adjacent rooms to WumpusFree
check_stench(Status, State, NewState, [X,Y], [Wx,Wy], Stench, Adjacent, WumpusFree, NewWumpusFree, Been):-
	append_list([[X,Y]], WumpusFree, WumpusFree1),
	append_list(Adjacent, WumpusFree1, NewWumpusFree),
	NewState = State.

check_breeze([X,Y], Breeze, Adjacent, PitFree, NewPitFree):-
	(member([X,Y],Breeze) -> write('(You suddenly feel some breezes in this room, a pit might be nearby!)'), nl,
	append_list([[X,Y]], PitFree, NewPitFree);
	append_list([[X,Y]], PitFree, PitFree1),
	append_list(Adjacent, PitFree1, NewPitFree)).
	
%Only at [1,1]
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	Been == [], %If been is empty, means we are at [1,1] and already feels stench
	(fire(east, [X,Y], [Wx,Wy]) -> delete_list([status(wumpus,alive),arrow(2)], State, State0),
	append_list([status(wumpus,dead), arrow(1)], State0, NewState);
	write('Turning 90 degree to the left.'), nl,
	fire(north, [X,Y], [Wx,Wy]), delete_list([status(wumpus,alive),arrow(2),facing(agent,east)], State, State0),
	append_list([status(wumpus,dead),arrow(1),facing(agent,north)], State0, NewState)),
	append_list(Adjacent, WumpusFree, NewWumpusFree).
	
%Came from a visited WumpusFree adjacent, and found a not visited WumpusFree adjacent, means the other 2 rooms can have wumpus
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	member(H, WumpusFree), \+ member(H, Been), %If first adjacent is WumpusFree and not been
	kill(State, NewState, [X,Y], [Wx,Wy], Adjacent, Adjacent, WumpusFree, NewWumpusFree, Been, 1).

%Adjacent room found to be not WumpusFree, and this is not the last adjacent, continue thinking
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	\+ member(H, WumpusFree),
	T \= [], %If this is not the last adjacent room
	thinkWumpus(State, NewState, [X,Y], [Wx,Wy], T, Adjacent, WumpusFree, NewWumpusFree, Been, N).

%Adjacent room found to be not WumpusFree, and this is the last adjacent
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	\+ member(H, WumpusFree),
	T == [], %If this is the last adjacent room
	N = 1, %If all adjacent rooms are checked and only the room you came from is WumpusFree
	len(Adjacent,L),
	(L == 4 -> NewWumpusFree = WumpusFree, NewState = State;
	kill(State, NewState, [X,Y], [Wx,Wy], Adjacent, Adjacent, WumpusFree, NewWumpusFree, Been, 1)).

%Count increment only when a WumpusFree adjacent is found to have been visited
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	member(H, WumpusFree), member(H, Been), %If first adjacent is WumpusFree and has been visited
	M is N + 1, M = 1,
	T \= [], %If this is not the last adjacent room
	thinkWumpus(State, NewState, [X,Y], [Wx,Wy], T, Adjacent, WumpusFree, NewWumpusFree, Been, M).

thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	member(H, WumpusFree), member(H, Been), %If first adjacent is WumpusFree and has been
	M is N + 1, M = 1,
	T == [], %If this is the last adjacent room
	len(Adjacent,L),
	(L == 4 -> NewWumpusFree = WumpusFree, NewState = State;
	kill(State, NewState, [X,Y], [Wx,Wy], Adjacent, Adjacent, WumpusFree, NewWumpusFree, Been, 1)).
	
%Else, if 2 or more adjacent rooms have been visited, means the other 2(or 1) room have wumpus
thinkWumpus(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, N):-
	member(H, WumpusFree), member(H, Been), %If first adjacent is WumpusFree and has been visited
	kill(State, NewState, [X,Y], [Wx,Wy], Adjacent, Adjacent, WumpusFree, NewWumpusFree, Been, 1).
	
add_adjacent([X,Y], Adjacent, N):-
	X<N, X>1, Y<N, Y>1, 
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1, 
	append_list([[X,Y2]], Adjacent1, Adjacent2), Y2 is Y - 1,
	append_list([[X,Y3]], Adjacent2, Adjacent3), Y3 is Y + 1,
	append_list([[X3,Y]], Adjacent3, Adjacent), X3 is X + 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=1, Y=1, 
	append_list([[X2,Y]], [], Adjacent1), X2 is X + 1,
	append_list([[X,Y2]], Adjacent1, Adjacent), Y2 is Y + 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=1, Y=N, 
	append_list([[X,Y2]], [], Adjacent1), Y2 is Y - 1,
	append_list([[X2,Y]], Adjacent1, Adjacent), X2 is X + 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=N, Y=1, 
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1,
	append_list([[X,Y2]], Adjacent1, Adjacent), Y2 is Y + 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=N, Y=N, 
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1,
	append_list([[X,Y2]], Adjacent1, Adjacent), Y2 is Y - 1.
	
add_adjacent([X,Y], Adjacent, N):-
	Y=N,
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1,
	append_list([[X3,Y]], Adjacent1, Adjacent2), X3 is X + 1,
	append_list([[X,Y2]], Adjacent2, Adjacent), Y2 is Y - 1.
	
add_adjacent([X,Y], Adjacent, N):-
	Y=1,
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1,
	append_list([[X3,Y]], Adjacent1, Adjacent2), X3 is X + 1,
	append_list([[X,Y2]], Adjacent2, Adjacent), Y2 is Y + 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=N,
	append_list([[X2,Y]], [], Adjacent1), X2 is X - 1,
	append_list([[X,Y2]], Adjacent1, Adjacent2), Y2 is Y + 1,
	append_list([[X,Y3]], Adjacent2, Adjacent), Y3 is Y - 1.
	
add_adjacent([X,Y], Adjacent, N):-
	X=1,
	append_list([[X,Y3]], [], Adjacent1), Y3 is Y - 1,	
	append_list([[X,Y2]], Adjacent1, Adjacent2), Y2 is Y + 1,
	append_list([[X2,Y]], Adjacent2, Adjacent), X2 is X + 1.

add_breezes(State, Breeze, NewBreeze, N):-
	member(at(pit,[Px,Py]), State), 
	delete_list([at(pit,[Px,Py])], State, Temp),
	add_breeze([Px,Py], Breeze, NewBreeze0, N),
	add_breezes(Temp, NewBreeze0, NewBreeze, N).
	
add_breezes(State, Breeze, NewBreeze, N):-
	NewBreeze = Breeze.

add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px<N, Px>1, Py<N, Py>1, 
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1, 
	append_list([[Px3,Py]], Breeze1, Breeze2), Px3 is Px + 1,
	append_list([[Px,Py2]], Breeze2, Breeze3), Py2 is Py - 1,
	append_list([[Px,Py3]], Breeze3, Breeze), Py3 is Py + 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=1, Py=1, 
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px + 1,
	append_list([[Px,Py2]], Breeze1, Breeze), Py2 is Py + 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=1, Py=N, 
	append_list([[Px,Py2]], Breeze0, Breeze1), Py2 is Py - 1,
	append_list([[Px2,Py]], Breeze1, Breeze), Px2 is Px + 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=N, Py=1, 
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1,
	append_list([[Px,Py2]], Breeze1, Breeze), Py2 is Py + 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=N, Py=N, 
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1,
	append_list([[Px,Py2]], Breeze1, Breeze), Py2 is Py - 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Py=N,
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1,
	append_list([[Px3,Py]], Breeze1, Breeze2), Px3 is Px + 1,
	append_list([[Px,Py2]], Breeze2, Breeze), Py2 is Py - 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Py=1,
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1,
	append_list([[Px3,Py]], Breeze1, Breeze2), Px3 is Px + 1,
	append_list([[Px,Py2]], Breeze2, Breeze), Py2 is Py + 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=N,
	append_list([[Px2,Py]], Breeze0, Breeze1), Px2 is Px - 1,
	append_list([[Px,Py2]], Breeze1, Breeze2), Py2 is Py + 1,
	append_list([[Px,Py3]], Breeze2, Breeze), Py3 is Py - 1.
	
add_breeze([Px,Py], Breeze0, Breeze, N):-
	Px=1,
	append_list([[Px,Py2]], Breeze0, Breeze1), Py2 is Py + 1,
	append_list([[Px2,Py]], Breeze1, Breeze2), Px2 is Px + 1,
	append_list([[Px,Py3]], Breeze2, Breeze), Py3 is Py - 1.

%If head is not WumpusFree, try to kill wumpus. If wumpus is not in head, search the adjacent list again	
kill(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, A):-
	\+ member(H, WumpusFree), member(facing(agent,D), State),
	[X_adj,Y_adj] = H,
	(((X_adj > X) ->
	(D == north -> rightturn(D, State, State1), 
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == south -> leftturn(D, State, State1),
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == west -> rightturn(D, State, State1), member(facing(agent,D1), State1), rightturn(D1, State1, State2),
	delete_list([status(wumpus,alive),arrow(2)], State2, State3),
	append_list([status(wumpus,dead),arrow(A)], State3, NewState);
	delete_list([status(wumpus,alive),arrow(2)], State, State1),
	append_list([status(wumpus,dead),arrow(A)], State1, NewState)),
	fire(east, [X,Y], [Wx,Wy]);
	(X_adj < X) ->
	(D == south -> rightturn(D, State, State1), 
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == north -> leftturn(D, State, State1),
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == east -> rightturn(D, State, State1), member(facing(agent,D1), State1), rightturn(D1, State1, State2),
	delete_list([status(wumpus,alive),arrow(2)], State2, State3),
	append_list([status(wumpus,dead),arrow(A)], State3, NewState);
	delete_list([status(wumpus,alive),arrow(2)], State, State1),
	append_list([status(wumpus,dead),arrow(A)], State1, NewState)),
	fire(west, [X,Y], [Wx,Wy]);
	(Y_adj > Y) ->
	(D == west -> rightturn(D, State, State1), 
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == east -> leftturn(D, State, State1),
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == south -> rightturn(D, State, State1), member(facing(agent,D1), State1), rightturn(D1, State1, State2),
	delete_list([status(wumpus,alive),arrow(2)], State2, State3),
	append_list([status(wumpus,dead),arrow(A)], State3, NewState);
	delete_list([status(wumpus,alive),arrow(2)], State, State1),
	append_list([status(wumpus,dead),arrow(A)], State1, NewState)),
	fire(north, [X,Y], [Wx,Wy]);
	(Y_adj < Y) ->
	(D == east -> rightturn(D, State, State1), 
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == west -> leftturn(D, State, State1),
	delete_list([status(wumpus,alive),arrow(2)], State1, State2),
	append_list([status(wumpus,dead),arrow(A)], State2, NewState);
	D == north -> rightturn(D, State, State1), member(facing(agent,D1), State1), rightturn(D1, State1, State2),
	delete_list([status(wumpus,alive),arrow(2)], State2, State3),
	append_list([status(wumpus,dead),arrow(A)], State3, NewState);
	delete_list([status(wumpus,alive),arrow(2)], State, State1),
	append_list([status(wumpus,dead),arrow(A)], State1, NewState)),
	fire(south, [X,Y], [Wx,Wy])) -> append_list(Adjacent, WumpusFree, NewWumpusFree);
	A2 is A - 1, %Decrement number of arrows by 1 if the above attempt failed
	kill(State, NewState, [X,Y], [Wx,Wy], T, Adjacent, WumpusFree, NewWumpusFree, Been, A2)).

%If head is WumpusFree, continue to search the adjacent list
kill(State, NewState, [X,Y], [Wx,Wy], [H|T], Adjacent, WumpusFree, NewWumpusFree, Been, A):-
	kill(State, NewState, [X,Y], [Wx,Wy], T, Adjacent, WumpusFree, NewWumpusFree, Been, A).
	
fire(Direction, [X,Y], [Wx,Wy]):-
	Direction == east,
	write('(Fires an arrow to the East direction.)'), nl,
	X2 is X + 1, X2 == Wx, Y == Wy,
	write('Wumpus: Kyahhhh!!! (A scream came from East indicates death of a stinky wumpus.)'), nl.
	
fire(Direction, [X,Y], [Wx,Wy]):-
	Direction == north,
	write('(Fires an arrow to the North direction.)'), nl,
	Y2 is Y + 1, Y2 == Wy, X == Wx,
	write('Wumpus: Kyahhhh!!! (A scream came from North indicates death of a stinky wumpus.)'), nl.
	
fire(Direction, [X,Y], [Wx,Wy]):-
	Direction == west,
	write('(Fires an arrow to the West direction.)'), nl,
	X2 is X - 1, Y == Wy, X2 == Wx,
	write('Wumpus: Kyahhhh!!! (A scream came from West indicates death of a stinky wumpus.)'), nl.
	
fire(Direction, [X,Y], [Wx,Wy]):-
	Direction == south,
	write('(Fires an arrow to the South direction.)'), nl,
	Y2 is Y - 1, Y2 == Wy, X == Wx,
	write('Wumpus: Kyahhhh!!! (A scream came from South indicates death of a stinky wumpus.)'), nl.
	
%Checks if head of adjacent list has not been visited yet, and is WumpusFree and Pitfree. If so, choose it as next step
priority_direction([X,Y], State, NewState, [H|T], Been, WumpusFree, PitFree):-
	[X2,Y2] = H,
	\+ member(H, Been), member(H, WumpusFree), member(H, PitFree), member(facing(agent,D), State),
	(X2 > X -> 	
	(D == north -> rightturn(D, State, NewState);
	D == west -> rightturn(D, State, State1), member(facing(agent,D2), State1), rightturn(D2, State1, NewState);
	D == south -> leftturn(D, State, NewState);
	delete_list([facing(agent,D)], State, State1),
	append_list([facing(agent,east)], State1, NewState));
	
	X2 < X -> 
	(D == north -> leftturn(D, State, NewState);
	D == east -> rightturn(D, State, State1), member(facing(agent,D2), State1), rightturn(D2, State1, NewState);
	D == south -> rightturn(D, State, NewState);
	delete_list([facing(agent,D)], State, State1),
	append_list([facing(agent,west)], State1, NewState));
	
	Y2 > Y -> 
	(D == east -> leftturn(D, State, NewState);
	D == south -> rightturn(D, State, State1), member(facing(agent,D2), State1), rightturn(D2, State1, NewState);
	D == west -> rightturn(D, State, NewState);
	delete_list([facing(agent,D)], State, State1),
	append_list([facing(agent,north)], State1, NewState));
	
	(D == west -> leftturn(D, State, NewState);
	D == north -> rightturn(D, State, State1), member(facing(agent,D2), State1), rightturn(D2, State1, NewState);
	D == east -> rightturn(D, State, NewState);
	delete_list([facing(agent,D)], State, State1),
	append_list([facing(agent,south)], State1, NewState))).

priority_direction([X,Y], State, NewState, [H|T], Been, WumpusFree, PitFree):-
	(T \= [] -> priority_direction([X,Y], State, NewState, T, Been, WumpusFree, PitFree);
	NewState = State).
	
leftturn(D, State, NewState):-
	write('(Make a 90 degree turn to your left)'), nl,
	delete_list([facing(agent,D)], State, State1),
	(D == east -> append_list([facing(agent,north)], State1, NewState), write('(Now facing: North)'), nl;
	D == south -> append_list([facing(agent,east)], State1, NewState), write('(Now facing: East)'), nl;
	D == west -> append_list([facing(agent,south)], State1, NewState), write('(Now facing: South)'), nl;
	append_list([facing(agent,west)], State1, NewState), write('(Now facing: West)'), nl).
	
rightturn(D, State, NewState):-
	write('(Make a 90 degree turn to your right)'), nl,
	delete_list([facing(agent,D)], State, State1),
	(D == east -> append_list([facing(agent,south)], State1, NewState), write('(Now facing: South)'), nl;
	D == south -> append_list([facing(agent,west)], State1, NewState), write('(Now facing: West)'), nl;
	D == west -> append_list([facing(agent,north)], State1, NewState), write('(Now facing: North)'), nl;
	append_list([facing(agent,east)], State1, NewState), write('(Now facing: East)'), nl).
	
is_subset([H|T], Set):-
    member(H, Set),
    is_subset(T, Set).
is_subset([], _).

delete_list([H|T], List, Final):-
    remove(H, List, Remainder),
    delete_list(T, Remainder, Final).
delete_list([], List, List).
    
remove(X, [X|T], T).
remove(X, [H|T], [H|R]):-
    remove(X, T, R).

write_sol([]).
write_sol([H|T]):-
    write_sol(T),
    write(H), nl.
                 
append_list([H|T], L1, [H|L2]):-
	append_list(T, L1, L2).
	
append_list([], L, L).

member(X, [X|_]).
member(X, [_|T]):-
    member(X, T).
	
len([],0). 
len([_|T],N):-
	len(T,X), N is X + 1.