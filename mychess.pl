
% ---------------------------------------------------------------------
% chess.pl
% Starter skeleton for "Reduced Chess: declarative KR & reasoning in Prolog"
% - Reduced piece set: king, rook, bishop, knight
% - Squares as atoms: a1 .. h8
% - Position represented as a list of piece(Color,Type,Square).
% ---------------------------------------------------------------------
%
% Implemented (useful helpers + movement rules for knight, rook, bishop, king),
% plus attacks/in_check/move_result/legal_move and an explainable illegal/2.
%
% This file is intentionally readable and mostly "pure" Prolog (no assert/retract).
%
% Usage:
% ?- [ 'chess.pl' ].
% ?- load_files('chess.pl').  % or use consult/1
% ?- legal_move(Pos, move(white, a1, a2, rook)).
%
% ---------------------------------------------------------------------

% ---------- Basic board / coordinates ----------

% on_board(+Square)
on_board(Sq) :- atom(Sq), atom_chars(Sq, [FileChar, RankChar]),
                file_char(FileChar, Fx), rank_char(RankChar, Fy),
                between(1,8,Fx), between(1,8,Fy).

file_char(FileChar, N) :- char_code(FileChar, C), N is C - 96.  % 'a'->1
rank_char(RankChar, N) :- char_code(RankChar, C), N is C - 48.  % '1'->1

square_to_coord(Sq, X, Y) :- atom_chars(Sq, [FileChar, RankChar]),
                             file_char(FileChar, X), rank_char(RankChar, Y).

coord_to_square(X, Y, Sq) :- X>=1, X=<8, Y>=1, Y=<8,
                            FileCode is X + 96, RankCode is Y + 48,
                            char_code(FileChar, FileCode), char_code(RankChar, RankCode),
                            atom_chars(Sq, [FileChar, RankChar]).

% ---------- Position helpers ----------
% Position is a list of piece(Color,Type,Square) terms.
occupied(Pos, Square, piece(Color,Type,Square)) :- member(piece(Color,Type,Square), Pos).
piece_at(Pos, Color, Type, Square) :- member(piece(Color,Type,Square), Pos).

opponent(white, black).
opponent(black, white).

% ---------- Path helpers for sliding pieces ----------

sign(0, 0).
sign(N, 1) :- N > 0.
sign(N, -1) :- N < 0.

% path_clear(+Pos, +From, +To) - true if all squares strictly between From and To are empty
path_clear(Pos, From, To) :-
    square_to_coord(From, Fx, Fy),
    square_to_coord(To,   Tx, Ty),
    DX is Tx - Fx, DY is Ty - Fy,
    step_vector(DX, DY, SX, SY), % computes unit step (valid only for sliding/straight/diagonal)
    (SX =:= 0, SY =:= 0 -> false ; % same square
    next_coord(Fx, Fy, SX, SY, Nx, Ny),
    path_clear_loop(Pos, Nx, Ny, Tx, Ty, SX, SY)).

% compute unit step if move is straight or diagonal
step_vector(DX, DY, SX, SY) :-
    (DX =:= 0, DY =\= 0 -> SX = 0, sign(DY, SY);
     DY =:= 0, DX =\= 0 -> SY = 0, sign(DX, SX);
     abs(DX) =:= abs(DY) -> sign(DX, SX), sign(DY, SY)
    ).

next_coord(X, Y, SX, SY, NX, NY) :- NX is X + SX, NY is Y + SY.

path_clear_loop(_, X, Y, Tx, Ty, _, _) :- X =:= Tx, Y =:= Ty, !, true. % reached target exactly -> path (excluding target) was empty
path_clear_loop(Pos, X, Y, Tx, Ty, SX, SY) :-
    (X =:= Tx, Y =:= Ty) -> true ;
    (coord_to_square(X, Y, S), \+ occupied(Pos, S, _),
     NX is X + SX, NY is Y + SY,
     path_clear_loop(Pos, NX, NY, Tx, Ty, SX, SY)).

% ---------- Movement shapes (geometry-only checks) ----------

% move_shape_ok(+From, +To, +PieceType)
move_shape_ok(From, To, knight) :-
    square_to_coord(From, Fx, Fy), square_to_coord(To, Tx, Ty),
    DX is Tx - Fx, DY is Ty - Fy,
    AbsDX is abs(DX), AbsDY is abs(DY),
    ((AbsDX =:= 1, AbsDY =:= 2) ; (AbsDX =:= 2, AbsDY =:= 1)).

move_shape_ok(From, To, king) :-
    square_to_coord(From, Fx, Fy), square_to_coord(To, Tx, Ty),
    DX is abs(Tx - Fx), DY is abs(Ty - Fy),
    DX =< 1, DY =< 1, (DX + DY) > 0.  % not same square

move_shape_ok(From, To, rook) :-
    square_to_coord(From, Fx, Fy), square_to_coord(To, Tx, Ty),
    (Fx =:= Tx ; Fy =:= Ty), \+ (Fx =:= Tx, Fy =:= Ty).

move_shape_ok(From, To, bishop) :-
    square_to_coord(From, Fx, Fy), square_to_coord(To, Tx, Ty),
    DX is abs(Tx - Fx), DY is abs(Ty - Fy), DX =:= DY, DX > 0.

% ---------- Pseudo-legal moves (geometry + simple occupancy) ----------

% pseudo_legal_move(+Pos, -Move)
% Move is move(Color, From, To, Type)
pseudo_legal_move(Pos, move(Color, From, To, knight)) :-
    piece_at(Pos, Color, knight, From),
    move_shape_ok(From, To, knight),
    on_board(To),
    \+ piece_at(Pos, Color, _, To).  % destination not occupied by same color

pseudo_legal_move(Pos, move(Color, From, To, king)) :-
    piece_at(Pos, Color, king, From),
    move_shape_ok(From, To, king),
    on_board(To),
    \+ piece_at(Pos, Color, _, To).

pseudo_legal_move(Pos, move(Color, From, To, rook)) :-
    piece_at(Pos, Color, rook, From),
    move_shape_ok(From, To, rook),
    on_board(To),
    \+ piece_at(Pos, Color, _, To),
    path_clear(Pos, From, To).

pseudo_legal_move(Pos, move(Color, From, To, bishop)) :-
    piece_at(Pos, Color, bishop, From),
    move_shape_ok(From, To, bishop),
    on_board(To),
    \+ piece_at(Pos, Color, _, To),
    path_clear(Pos, From, To).

% ---------- Move result (functional successor position) ----------
% move_result(+Pos, +Move, -NewPos)
move_result(Pos, move(Color, From, To, Type), NewPos) :-
    % remove the moving piece at From
    ( select(piece(Color, Type, From), Pos, PosWithoutFrom) -> true ; PosWithoutFrom = Pos ),
    % remove any opponent piece at To
    ( select(piece(_,_,To), PosWithoutFrom, PosWithoutTo) -> true ; PosWithoutTo = PosWithoutFrom ),
    % add the moved piece at To
    NewPos = [ piece(Color, Type, To) | PosWithoutTo ].

% ---------- Attacks and check ----------
% attacks(+Pos, +Color, +Square) - true if Color can attack Square (capture or move there)
attacks(Pos, Color, Square) :-
    member(piece(Color, Type, From), Pos),
    % Use geometry + occupancy rules encoded in pseudo_legal_move. pseudo_legal_move excludes dest occupied by same color,
    % which matches the "attacks" convention (do not say you attack your own squares).
    pseudo_legal_move(Pos, move(Color, From, Square, Type)).

% in_check(+Pos, +Color) - true if Colors king is attacked by any opponent piece
in_check(Pos, Color) :-
    piece_at(Pos, Color, king, KingSq),
    opponent(Color, Opp),
    attacks(Pos, Opp, KingSq).

% ---------- Legal move (king safety filter) ----------
% legal_move(+Pos, +Move) - Move is pseudo-legal and does not leave own king in check
legal_move(Pos, move(Color, From, To, Type)) :-
    pseudo_legal_move(Pos, move(Color, From, To, Type)),
    move_result(Pos, move(Color, From, To, Type), NewPos),
    \+ in_check(NewPos, Color).

% avoids_check is same as legal_move in this reduced project
avoids_check(Pos, Move) :- legal_move(Pos, Move).

any_move_avoids_check(Pos, Color) :-
    member(piece(Color, Type, From), Pos),
    % find any move of that piece that is legal
    ( Type = knight ; Type = king ; Type = rook ; Type = bishop ),
    % generate candidate To across board (brute force for simplicity)
    between(1,8,X), between(1,8,Y),
    coord_to_square(X, Y, To),
    Move = move(Color, From, To, Type),
    legal_move(Pos, Move), !.

% ---------- Explainability: illegal/2 ----------
% illegal(+Pos, +Move, -Reason) or illegal(+Move, -Reason) if Pos is included in the Move term
illegal(Pos, move(Color, From, To, Type), Reason) :-
    % 1) from empty?
    ( \+ piece_at(Pos, _, _, From) -> Reason = from_empty, ! ;
      true ),
    % 2) to out of board?
    ( \+ on_board(To) -> Reason = to_out_of_board, ! ;
      true ),
    % 3) destination occupied by same color?
    ( piece_at(Pos, Color, _, To) -> Reason = destination_occupied_by_same_color, ! ;
      true ),
    % 4) shape invalid?
    ( \+ move_shape_ok(From, To, Type) -> Reason = illegal_move_shape, ! ;
      true ),
    % 5) blocked (shape ok but path not clear for sliding)
    ( (Type = rook ; Type = bishop) ,
      \+ path_clear(Pos, From, To) -> Reason = blocked, ! ;
      true ),
    % 6) king moving into attacked square?
    ( Type = king,
      opponent(Color, Opp),
      attacks(Pos, Opp, To) -> Reason = square_attacked, ! ;
      true ),
    % 7) would leave king in check?
    ( pseudo_legal_move(Pos, move(Color, From, To, Type)),
      move_result(Pos, move(Color, From, To, Type), NewPos),
      in_check(NewPos, Color) -> Reason = would_leave_king_in_check, ! ;
      true ),
    % 8) default fallback
    ( \+ pseudo_legal_move(Pos, move(Color, From, To, Type)) -> Reason = illegal_move_other ; Reason = unknown ).

% convenience wrapper when Move includes Pos as first arg: illegal(Move, Reason) where Move = move(Pos, ...)
illegal(move(_,_,_,_), Reason) :-
    % fallback: we cannot determine Pos; user should call illegal(Pos, Move, Reason)
    Reason = no_position_provided.

% ---------- End of chess.pl ----------
