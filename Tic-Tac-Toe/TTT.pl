% --- Player ---
player(x).
player(o).

other(x, o).
other(o, x).

% --- board c(Row,Col) 1..3 ---
cell(c(R,C)) :- between(1,3,R), between(1,3,C).

state([mark(c(1,1),x), mark(c(2,2),o)], x).

initial_state(state([], x)).

% true if cell is occupied in board
occipied(Board, Cell) :- member(mark(Cell,_), Board).

% true if cell is empty in board
empty(Board, Cell) :-
    cell(Cell),
    \+ occipied(Board, Cell).

% legal move
legal_move(state(Board, Turn), Cell, Turn) :-
    empty(Board, Cell).

% transition
make_move(state(Board, Turn), Cell, state(NewBoard, NextTurn)) :-
    legal_move(state(Board, Turn), Cell, Turn),
    NewBoard = [mark(Cell, Turn) | Board],
    other(Turn, NextTurn).

% --- Winning ---
% rows
line([c(1,1), c(1,2), c(1,3)]).
line([c(2,1), c(2,2), c(2,3)]).
line([c(3,1), c(3,2), c(3,3)]).

% columns
line([c(1,1), c(2,1), c(3,1)]).
line([c(1,2), c(2,2), c(3,2)]).
line([c(1,3), c(2,3), c(3,3)]).

% diagonals
line([c(1,1), c(2,2), c(3,3)]).
line([c(1,3), c(2,2), c(3,1)]).

has_mark(Board, Cell, Player) :-
    member(mark(Cell, Player), Board).

wins(state(Board,_), Player) :-
    player(Player),
    line(Cells),
    forall(member(C, Cells), has_mark(Board, C,  Player)).

% game over
full(Board) :-
    \+ (cell(Cell), empty(Board, Cell)).

game_over(State) :-
    wins(State, x);
    wins(State, o);
    State = state(Board,_), full(Board).

% draw
draw(state(Board,_)) :-
    full(Board),
    \+ wins(state(Board,_), x),
    \+ wins(state(Board,_), o).

% --- Legal Moves ---
legal_moves(state(Board, Turn), Moves) :-
    findall(Cell, (cell(Cell), empty(Board, Cell)), Moves),
    player(Turn).

play_moves(State. [], State).
play_moves(State, [Cell|Rest], FinalState) :-
    make_move(State, Cell, Next),
    play_moves(Next, Rest, FinalState).


