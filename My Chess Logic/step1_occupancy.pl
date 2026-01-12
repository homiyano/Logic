% A position is a list of piece(Color, Type, Square).

% occupied(Position, Square) is true if there exists a piece on Square.
occupied(Pos, Sq) :-
    member(piece(_, _, Sq), Pos).
