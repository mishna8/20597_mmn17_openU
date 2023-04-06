%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%  Mamam17 -  20596 - Prolog & Artificial Intelligence  %%%%%%
%  michal naftulina 
%  connect4.pl
%  4 connect game - 6X7 board - human vs AI - minimax with alpha beta pruning
%  start the game by entering "?-start."
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% dynamic utllity predicates & data structures  for interal representation
:- dynamic top/2.       % for every column hold the possible playing hieght = top(column,hieght)
:- dynamic board/2.	% holds the boards dimentions to work with (6X7) = board(columns,rows)
:- dynamic piece/3.	% for every cell hold what piece is placed there if any = piece(column,row,red_or_yellow)
:- dynamic win/1.	% indicator to which player the board has a 4 in row 
:- dynamic lose/1.	% indcator which is the player who loses if the other wins
:- dynamic val/3.	% for every cell hold the value for the alpha-beta = val(columne,row,value)
:- dynamic score/2.	% holds the value of each players moves during the game and used to calculate next move value - score(Color,Value)
:- dynamic level/1.	%the difficulty of the game, meaning the depth of the tree in the alpha beta
:- dynamic player/1.	% hold the current move of the human player

% players representation 
score(r,0). % human
score(y,0). % pc

%%%%%%%%%  clean-up and initialize the board  %%%%%%%%%%%%%%%%%%%%%%%%

% cleans the top of all columns
reset_top(0):-!.
reset_top(C):-assert(top(C,0)),NC is C-1, reset_top(NC) .

% cleans the win/lose
resetStatus():-
	retractall(win(_)),
	retractall(lose(_)).

% cleans all game settings
prep_board(C,R):-
	retractall(board(_,_)),
	retractall(level(_)),
	retractall(score(_,_)),
	assert(score(r,0)),
	assert(score(y,0)),
	assert(board(C,R)),
	retractall(top(_,_)),
	reset_top(C),
	retractall(piece(_,_,_)),
	retractall(player(_)),
	resetStatus().

%%%%%%%%%%%%  Start the Game %%%%%%%%%%%%%%

% this is the command that starts the game
start:-
	prep_board(6,7),
	createValGride(), 	% this grid will not change, but will serve to calculate the pc's moves
	(  
	        writeln('Select the difficulty of the game: [choose a number 1 - 5 ]'),
	        read(N),
	        assert(level(N))
	    
	),
	writeln('on your turn select the column you wish to play: [choose a number 1 - 6 ]'),
	game().

% the game itself, first check board if there's a draw
game():-
	draw(),writeln('Draw Situation'),!.

% if no draw, get player's move, check if they can win
game():-
	write('\n'),writeln('\nYOUR TURN:  '),
	getMove(r),	% get player's column, check for errors, and check for win
	win(r),
	writeln('\nYou Win!'),!.

% if not get computer's move, check if it can win, if not go again
game():-
	write('\n'),writeln('\nCOMPUTER TURN:  '),write('\n'),	
	level(N),CurN is N+1,
	player(M),remove(M),
	alphabeta(r,CurN,M,-1000,1000,Move,_),	% send the last player's move and state to next move y
	insert(M,r),insert(Move,y),
	write('\n'),print(),
	top(Move,H),		% check for win
	(
		isFour(Move,H),!,writeln('\nComputer win!');		
	        game()
	).

%%%%%%%%%  input/output  %%%%%%%%%%%%%

% input from human player
getMove(Color):-
	read(C),
	(
		full(C),!,(print(),writeln('\nFull'),getMove(Color)) ; 
		(	
			retractall(player(_)),assert(player(C)),
			insert(C,Color),
			print(),
			top(C,R),	%check for win
			(checkStatus(C,R,Color);!),!
		)
	).
getMove(Color):-
	writeln('\nInvalid'),getMove(Color).


% write each player's own peice 
write_piece(r):-write('[o]').
write_piece(y):-write('[x]').

% output, print the board 
print():- 
	board(_,Y),
	print(1,Y).

print(C,R):-	
	board(X,_),
	C == X,
	R == 1,
	(	% if has a players's peice write it else write a space
		piece(C,R,Color),!,write_piece(Color); 
		write('[_]')
	).

print(C,R):-
	board(X,_),
	NewC is C+1,
	(	% if has a players's peice write it else write a space
		piece(C,R,Color),!,write_piece(Color); 
		write('[_]')
	),
	C < X ,
	print(NewC,R),!.

print(_,R):-
	NewR is R-1 ,
	writeln(''),
	R >= 0,
	print(1,NewR).

%%%%%%%%%%%  make moves  %%%%%%%%%%%%%

% insert a piece to the column C of the player Color
insert(C,Color):-
	not(full(C)),
	top(C,H),				% get old height
	TmpH is H + 1,
        retractall(top(C,_)),			% remove old height
        assert(top(C,TmpH)),			% set the new height
        assert(piece(C,TmpH,Color)),		% mark the player's peice in the cell
	score(Color,V),				% get old score
	val(C,TmpH,E), 				% get cell's value
	V1 is V + E, 				% make new score
	retractall(score(Color,_)), 		% remove old score
	assert(score(Color,V1)); 		% set new score
	true.

% remove the top piece of the column C
% used by the alphabeta function to go back on paths when checking for best solution
remove(C):-
	top(C,H),				% get new height	
	piece(C,H,Color),			% get the peice in the cell
	retractall(piece(C,H,_)),		% remove it
	TmpH is H - 1,
        retractall(top(C,_)),			% remove new height
        assert(top(C,TmpH)),			% set back the old height
	score(Color,V),				% get new score
	val(C,H,E),				% get cell value
	V1 is V - E,				% revert the score
	retractall(score(Color,_)),		% remove new score
	assert(score(Color,V1));		% set back the old score
	true.

%%%%  Check game status  %%%%%%%%%%%%

% if any of the colors win set win/lose state
checkStatus(X,Y,Color):-
	isFour(X,Y),
	(   Color == r ,!, assert(win(r)),assert(lose(y));
	    Color == y ,!, assert(win(y)),assert(lose(r))
	).
checkStatus(_,_,_).

% check if the board is full if all columns has a piece at the very top 
draw():-
	board(C,R),
	aggregate_all(count,top(_,R),C1),
	C1 == C.

% check if the column available to drop peices 
full(C):- 
	board(_,X),
	piece(C,X,_).

% continue compering peices in the given direction (incC,incR) and counting until blocked 
checkPath(C,R,IncC,IncR,Res):-
       NewC is C + IncC,
       NewR is R + IncR,
       piece(C,R,C1),
       piece(NewC,NewR,C2),
       C1 == C2,
       checkPath(NewC,NewR,IncC,IncR,Res1),
       Res is Res1 + 1,!.	
checkPath(_,_,_,_,Res):-
	Res is 1.

% if from given cell there is a path of 4 peices there is a win
% Horzintal Check
isFour(X,Y):-
	checkPath(X,Y,1,0,R1),
	checkPath(X,Y,-1,0,R2),
	R is R1 + R2 - 1  ,
	R >= 4,!.
% Vertical Check
isFour(X,Y):-
	checkPath(X,Y,0,1,R1),
	checkPath(X,Y,0,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.
% Main Diagonal Check 
isFour(X,Y):-
	checkPath(X,Y,1,1,R1),
	checkPath(X,Y,-1,-1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.
% Secondary Diagonal Check
isFour(X,Y):-
	checkPath(X,Y,1,-1,R1),
	checkPath(X,Y,-1,1,R2),
	R is R1 + R2 - 1,
	R >= 4,!.

%%%%%%%%%%%%%  Minimax alphabeta pruning  %%%%%%%%%%%%%%

static(Color,P,V):- 
	insert(P,Color),
	score(r,Vr),score(y,Vy),V is Vy - Vr, 
	resetStatus(),remove(P).

% the main ai algorithem to calculate pc's move
alphabeta(Color,Depth,P,A,B,BP,V):-
	Depth>0,!,(
			NewDep is Depth-1,
			(Color ==r,!,NewC = y; Color==y,!, NewC=r),
			board(C,_),
			setof(X,between(1,C,X),POSLIST), 
			insert(P,Color),	
			boundedbest(NewC,NewDep,POSLIST,A,B,BP,V),
			resetStatus(), 
			remove(P)
		);
	static(Color,P,V).

boundedbest(Color,Depth,[P|POSLIST],A,B,BP,V):-
	full(P),!,boundedbest( Color,Depth,POSLIST,A,B,BP,V);
	(
	top(P,H),
	checkStatus(P,H,Color), 
	    	(	win(r) ,!, V is -100 * (Depth + 1) ; 
			win(y) ,!, V is 100 * (Depth + 1)  ;
			draw() ,!, V is 0 ;	
			not(win(r);win(y);draw()),!,alphabeta(Color,Depth,P,A,B,_,V)
		),
  	goodenough(Color,Depth,POSLIST,A,B,P,V,BP,BV)).

goodenough(_,_,[],_,_,P,V,P,V):- !.

goodenough(Color,_,_,A,B,P,V,P,V):-
	Color == y ,V>B,!; % min(P)
	Color == r ,V<A,!. % max(P)

goodenough(Color,Depth,POSLIST,A,B,P,V,BP,BV):-
	newbounds(Color,A,B,V,NEWA,NEWB), 
	boundedbest(Color,Depth,POSLIST,NEWA,NEWB,P1,V1),
	betterof(Color,P,V,P1,V1,BP,BV).

newbounds(Color,A,B,V,V,B):- Color == y,V>A,!. % increased lower bound - max
newbounds(Color,A,B,V,A,V):- Color == r,V<B,!. % decreased upper bound - min
newbounds(_,A,B,_,A,B).

betterof(Color,P,V,P1,V1,P,V):- 
	Color == y,V>V1,!; % max
	Color == r,V<V1,!. % min 
betterof(_,_,_,P1,V1,P1,V1). 


%%%%%%%%%%%%  Evaluation Rules %%%%%%%%

% create the values of every cell 
createValGride():-
	board(_,R),
	retractall(val(_,_,_)),
	initValRow(R),
	getValRow(R).


initValCol(_,0):-!.
initValCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	initValCol(CurRow,NewCol),
	assert(val(CurRow,CurCol,0)).

initValRow(0):-!.
initValRow(CurRow):-
	NewRow is CurRow - 1,
	initValRow(NewRow),
	board(C,_),
	initValCol(CurRow,C).


getValCol(_,0):-!.
getValCol(CurRow,CurCol):-
	NewCol is CurCol - 1,
	getValCol(CurRow,NewCol),
	getHor(CurRow,CurCol),
	getVer(CurRow,CurCol),
	getPrimDia(CurRow,CurCol),
	getSecDia(CurRow,CurCol).

getValRow(0):-!.
getValRow(CurRow):-
	NewRow is CurRow - 1,
	getValRow(NewRow),
	board(C,_),
	getValCol(CurRow,C).

% add Horzintal 
getHor(X,Y):-
	(Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	val(X,Y,E),val(X,Y1,E1),val(X,Y2,E2),val(X,Y3,E3),
	Ne  is E+1 ,retractall(val(X,Y,E))  ,assert(val(X,Y,Ne)),
	Ne1 is E1+1,retractall(val(X,Y1,E1)),assert(val(X,Y1,Ne1)),
	Ne2 is E2+1,retractall(val(X,Y2,E2)),assert(val(X,Y2,Ne2)),
	Ne3 is E3+1,retractall(val(X,Y3,E3)),assert(val(X,Y3,Ne3)));
	true.

% add Vertical 
getVer(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	val(X,Y,E),val(X1,Y,E1),val(X2,Y,E2),val(X3,Y,E3),
	Ne  is E+1 ,retractall(val(X,Y,E))  ,assert(val(X,Y,Ne)),
	Ne1 is E1+1,retractall(val(X1,Y,E1)),assert(val(X1,Y,Ne1)),
	Ne2 is E2+1,retractall(val(X2,Y,E2)),assert(val(X2,Y,Ne2)),
	Ne3 is E3+1,retractall(val(X3,Y,E3)),assert(val(X3,Y,Ne3)));
	true.

% add Main Diagonal 
getPrimDia(X,Y):-
	(   X1 is X+1, X2 is X+2, X3 is X+3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	val(X,Y,E),val(X1,Y1,E1),val(X2,Y2,E2),val(X3,Y3,E3),
	Ne  is E+1 ,retractall(val(X,Y,E))  ,assert(val(X,Y,Ne)),
	Ne1 is E1+1,retractall(val(X1,Y1,E1)),assert(val(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(val(X2,Y2,E2)),assert(val(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(val(X3,Y3,E3)),assert(val(X3,Y3,Ne3)));
	true.

% add Secondary Diagonal 
getSecDia(X,Y):-(
	X1 is X-1, X2 is X-2, X3 is X-3,
	Y1 is Y+1, Y2 is Y+2, Y3 is Y+3,
	val(X,Y,E),val(X1,Y1,E1),val(X2,Y2,E2),val(X3,Y3,E3),
	Ne  is E+1 ,retractall(val(X,Y,E))  ,assert(val(X,Y,Ne)),
	Ne1 is E1+1,retractall(val(X1,Y1,E1)),assert(val(X1,Y1,Ne1)),
	Ne2 is E2+1,retractall(val(X2,Y2,E2)),assert(val(X2,Y2,Ne2)),
	Ne3 is E3+1,retractall(val(X3,Y3,E3)),assert(val(X3,Y3,Ne3)));
        true.






