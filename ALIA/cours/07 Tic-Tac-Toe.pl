
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Based on Robert Pinchbeck Tic Tac Toe in Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic board/1.
:-dynamic player/2.

% The representation of the board is a fact with one list of 9 elements
% Its elements can be 'x', 'o' or 'e' for empty).

next_player(1,2).		%%% determines the next player after the given player
next_player(2,1). 

inverse_mark('x','o'). 	%%% determines the opposite of the given mark
inverse_mark('o','x'). 

player_mark(1,'x').		%%% the mark for the given player
player_mark(2,'o'). 
 
opponent_mark(1, 'o'). 	%%% the inverse mark of the given player
opponent_mark(2, 'x'). 

maximizing('x').	
% The player playing x is always trying to maximize board position utility
minimizing('o').	
% The player playing o is always trying to minimize board position utility

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jouer :- welcome,				%%% Display welcome message, initialize game 
			play(1),			%%% Play the game starting with player 1 
				goodbye.        %%% Display end of game message
jouer :- goodbye.

welcome :- initialize, nl, nl, write('Tic-Tac-Toe example'),
			read_players, output_players.

initialize :-	%%% random seed may use time to initialize random number generator
				retractall(player(_,_)), retractall(board(_)),	
				asserta(board(['e','e','e','e','e','e','e','e','e'])). 
				%%% create an empty board

goodbye :- 	board(Board), nl, nl, write('Game over: '), 
				output_winner(Board),
					retractall(player(_,_)), retractall(board(_)),	
						read_play_again(V), !, 
							(V == 'Y' ; V == 'y'), !, jouer.

read_play_again(V) :- nl, nl, write('Play again (Y/N)? '), read(V),
						(V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !.
read_play_again(V) :- nl, nl, write('Please enter Y or N.'),
						read_play_again(V).

read_players 
	:- nl, nl, write('Number of human players? '), read(N), set_players(N).

set_players(0) :- asserta(player(1,computer1)), asserta(player(2,computer2)), !.

set_players(1) :- nl, write('Is human playing x or o (x moves first)? '),
					read(M), human_playing(M), !.

set_players(2) :- asserta(player(1,human)), asserta(player(2,human)), !.
 
set_players(_) :- nl, write('Please enter 0, 1, or 2.'), read_players.

human_playing(M) :-
	(M == 'x' ; M == 'X'), 
		asserta(player(1,human)), asserta(player(2,computer1)), !.

human_playing(M) :-
	(M == 'o' ; M == 'O'),
		asserta(player(1,computer1)), asserta(player(2,human)), !.

human_playing(_) :- nl, write('Please enter X or O.'), set_players(1).
 
play(Player1) 
	:- 	board(Board), output_board(Board), 
			not(game_over(Player1, Board)), 
				make_move(Player1, Board), !,
					next_player(Player1,Player2), play(Player2).

%.......................................
% win
%.......................................
% Players win by having their mark M in one of the following configurations:
% 
win([M,M,M, _,_,_, _,_,_],M).
win([_,_,_, M,M,M, _,_,_],M).
win([_,_,_, _,_,_, M,M,M],M).
win([M,_,_, M,_,_, M,_,_],M).
win([_,M,_, _,M,_, _,M,_],M).
win([_,_,M, _,_,M, _,_,M],M).
win([M,_,_, _,M,_, _,_,M],M).
win([_,_,M, _,M,_, M,_,_],M).

%.......................................
% move
%.......................................
% It applies a move on the given board and it modifies the board
% (put Mark in Square on Board2
% 
move(Board1,Square,Mark,Board2) :- set_item(Board1,Square,Mark,Board2).

%.......................................
% set_item
%.......................................
% Given a list List1, it replaces the item at Position with Val 
% to get list List2
%
set_item(List1,Position,Val,List2) :-
	nth1(Position,List1,_,List3), nth1(Position,List2,Val,List3), !.

%.......................................
% game_over
%.......................................
% Game is over if opponent wins or if none of the squares are empty
%
game_over(Player,Board) :- opponent_mark(Player, Mark), win(Board, Mark), !.
game_over(_,Board) :- isFull(Board). 

isFull([]).
isFull([H|T]) :- H \== 'e', isFull(T).

%.......................................
% make_move
%.......................................
% It requests next move from human or computer,
% then it applies that move to the given board
% 
make_move(Player, Board1) 
	:- 	player(Player, Type_Joueur), 
			make_move2(Type_Joueur, Player, Board1, Board2), 
				retract(board(_)), asserta(board(Board2)).

make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('Player '), write(Player), write(' move? '), read(Square), 
			nth1(Square,Board1,'e'), !, player_mark(Player, Mark), 
				move(Board1,Square,Mark,Board2), !.
				
make_move2(human, Player, Board1, Board2) 
	:-	nl, nl, write('Please select a numbered square.'),
			make_move2(human,Player,Board1,Board2).

% A move computed thanks to minimax is made of values for the 3 variables 
% Mark, Square and Utility

make_move2(computer1, Player, Board1, Board2) 
	:-	nl, nl, write('Computer1 is thinking about next move ...'), nl,
			player_mark(Player, Mark),
				minimax(0, Board1, Mark, Square, _),
					move(Board1,Square,Mark,Board2), !,
		nl, nl, write('Computer1 places '), write(Mark), write(' in square '),
			write(Square), write('.').

make_move2(computer2, Player, Board1, Board2) 
	:-	nl, nl, write('Computer2 tries to think about next move ...'), nl,
			player_mark(Player, Mark),
				random_playing(Board1,Mark,Square),
					move(Board1,Square,Mark,Board2), !,
		nl, write('Computer2 places '), write(Mark), write(' in square '),
			write(Square), write('.').
		
random_playing(Board,Mark,Square) :- 
      repeat, random_between(1,9,Square), nth1(Square,Board,'e'), !.
	  
%.......................................
% possible_moves
%.......................................
% It retrieves a list of possible moves (empty squares) on a board.
% 
possible_moves(Board,List) 
	:-	not(win(Board,x)),	%%% if either player already won, 
							%%% then there are no available moves
		not(win(Board,o)),
		bagof(N, nth1(N,Board,'e'), List). % Fail if List should be empty
		
%.......................................
% utility
%.......................................
% It computes the value of a given board position
% 
utility(Board,1) :- win(Board,'x'), !.
utility(Board,-1) :- win(Board,'o'), !.
utility(_,0).

%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a draw, 
% so the algorithm is effectively playing not-to-lose. 

% For the opening move against an optimal player, the best 
% minimax can ever hope for is a draw. Technically speaking, 
% any opening move is acceptable. Save the user the trouble 
% of waiting for the computer to search the entire minimax tree
% by simply selecting a random square. 
 
minimax(Depth,['e','e','e','e','e','e','e','e','e'],Mark,Square,Utility) 
	:- random_between(1,9,Square), !.

minimax(Depth,Board,Mark,Square,Utility) :-
 Depth2 is Depth+1,
 possible_moves(Board,List), !,		%%% get the list of possible moves
	best(Depth2,Board,Mark,List,Square,Utility), !.	
					%%% recursively determine the best available move

% If there are no more available moves, then the minimax value is 
% the utility of the given board position 
 
minimax(Depth,Board,Mark,Square,Utility) :- utility(Board,Utility).

%.......................................
% best
%.......................................
% determines the best move in a given list of moves by 
% recursively calling minimax
% 
% if there is only one move left in the list... 

best(Depth,Board,Mark,[Square1],Square1,Utility) 
	:-	move(Board,Square1,Mark,Board2),	%%% apply that move to the board,
			inverse_mark(Mark,Mark2), !,
			%%% then recursively search for the utility of that move.
				minimax(Depth,Board2,Mark2,_,Utility), !,	 
				output_value(Depth,Square1,Utility), !.

% if there is more than one move in the list... 

best(Depth,Board,Mark,[Square1|Other_Moves],Square,Utility) 
	:-	move(Board,Square1,Mark,Board2),	%%% apply the first move (in the list)
			inverse_mark(Mark,Mark2), !,
				minimax(Depth,Board2,Mark2,_,Utility1),	
			%%% recursively search for the utility value of that move
			%%% and determine the best move of the remaining moves
				best(Depth,Board,Mark,Other_Moves,Square2,Utility2),	
				output_value(Depth,Square1,Utility1),
			better(Depth,Mark,Square1,Utility1,Square2,Utility2,Square,Utility). 	
	%%% choose the better of the two moves based on their utility values

%.......................................
% better
%.......................................
% returns the better of two moves based on their utility values.
%
% if both moves have the same utility value, then one is chosen at random. 
%
better(_,Mark,Square1,Utility1,Square2,Utility2,Square1,Utility1) 
	:-	maximizing(M),				%%% if the player is maximizing
		Utility1 > Utility2, !.		%%% then greater is better.

better(_,Mark,Square1,Utility1,Square2,Utility2,Square1,Utility1) 
	:-	minimizing(M),				%%% if the player is minimizing,
		Utility1 < Utility2, !.		%%% then lesser is better.
	
better(_,Mark,Square1,Utility1,Square2,Utility2,Square,Utility) 
	:-	Utility1 == Utility2,		%%% if moves have equal utility,
		random_between(1,10,R),		%%% then pick one of them at random
		better2(_,R,Mark,Square1,Utility1,Square2,Utility2,Square,Utility), !.

better(_,Mark,Square1,Utility1,Square2,Utility2,Square2,Utility2). 
									%%% otherwise, second move is better
	
%.......................................
% better2
%.......................................
% randomly selects among two squares of the same utility value
%
better2(_,R,Mark,Square1,Utility1,Square2,Utility2,Square1,Utility1) :- R < 6, !.
better2(_,R,Mark,Square1,Utility1,Square2,Utility2,Square2,Utility2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Output and dispay
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :-
	nl, player(1, Who1),
	write('Player 1 is '),	%%% either human or computer1 or computer2
	write(Who1), 
	nl, player(2, Who2),
	write('Player 2 is '),	%%% either human or computer1 or computer2
	write(Who2), ! .

 output_winner(Board) :- win(Board,x), write('X wins.'), !.
 output_winner(Board) :- win(Board,o), write('O wins.'), !.
 output_winner(Board) :- write('No winner: Draw').

 output_board(Board) :-
	nl, nl,
	output_square(Board,1), write('|'),
	output_square(Board,2), write('|'),
	output_square(Board,3), nl,
	write('-----------'), nl,
	output_square(Board,4), write('|'),
	output_square(Board,5), write('|'),
	output_square(Board,6), nl,
	write('-----------'), nl,
	output_square(Board,7), write('|'),
	output_square(Board,8), write('|'),
	output_square(Board,9), !.

output_board :- board(Board), output_board(Board), !.

output_square(Board,Square) :-
	nth1(Square,Board,Mark), write(' '), output_square2(Square,Mark), write(' '), !.
	
output_square2(Square, 'e') :-
	write(Square), !.	%%% if square is empty, output the square number

output_square2(Square, Mark) :-
	write(Mark), !.		%%% if square is marked, output the mark

output_value(1,Square,Utility) 
	:- nl, write('Square '), write(Square), write(', utility: '), write(Utility), !.
output_value(Depth,Square,Utility).
	
% Useful for debug
	
board0(['x','x','x','o','o','e','e','e','e']). 	% X wins
board1(['x','o','x','o','o','e','x','x','e']).	% not completed
board2(['x','x','o','o','x','x','o','o','x']).	% X wins
board3(['x','x','o','o','x','x','x','o','o']).	% draw
board4(['x','o','x','x','o','e','e','o','e']).	% O wins



