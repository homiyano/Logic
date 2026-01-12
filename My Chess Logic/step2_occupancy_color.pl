% occupied(Position, Square)
occupied(Pos, Sq) :-
    member(piece(_, _, Sq), Pos).

% occupied_by(Position, Color, Square)
occupied_by(Pos, Color, Sq) :-
    member(piece(Color, _, Sq), Pos).

% occupied_by_opponent(Position, Color, Square)
occupied_by_opponent(Pos, Color, Sq) :-
    member(piece(Other, _, Sq), Pos),
    Other \= Color.
