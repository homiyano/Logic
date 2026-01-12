
% ---------------------------------------------------------------------
% tests.pl
% Simple test harness for chess.pl starter file.
% Loads chess.pl and runs a series of checks printing expected vs. actual.
% ---------------------------------------------------------------------

:- ensure_loaded('mychess.pl').

% Example positions used in tests
pos1([ piece(white, king, e1),
       piece(white, rook, a1),
       piece(black, rook, e8) ]).

pos_blocked([ piece(white, rook, a1),
               piece(white, knight, a2) ]).

pos_knight_block([ piece(white, knight, b1),
                   piece(white, pawn, c3),   % pawn is a dummy piece type to block landing
                   piece(white, king, e1) ]).

pos_bishop_capture([ piece(white, bishop, c1),
                     piece(black, rook, f4) ]).

% Simple test runner utilities
ok(TestName) :- format('PASS: ~w~n', [TestName]).
fail(TestName, Expected, Actual) :- format('FAIL: ~w -- expected: ~w, got: ~w~n', [TestName, Expected, Actual]).

% --------------------- My Tests position ------------------------
% 1
pempty([piece(white, rook, a1)]).
pblock([piece(white, rook, a1), piece(white, knight, a2)]).

pbishop_block([ piece(white, bishop, c1),
                piece(white, knight, d2) ]).

pknight([ piece(white, knight, b1),
          piece(white, king, e1) ]).

% 2

pcheck_rook([ piece(white, king, e1),
              piece(black, rook, e8) ]).

pcheck_knight([ piece(white, king, e1),
                piece(black, knight, f3) ]).

pcheck_bishop([ piece(white, king, e1),
                piece(black, bishop, h4) ]).

% 3

% A) Pseudo-legal but illegal due to king exposure:
% White rook is shielding the king from a black rook. Moving the rook away exposes the king.
pexpose_king([
    piece(white, king, e1),
    piece(white, rook, e2),
    piece(black, rook, e8)
]).

% B) Blocking the checking piece:
% White is in check by black rook e8; move rook e2->e3 blocks the file.
pblock_check([
    piece(white, king, e1),
    piece(white, rook, e2),
    piece(black, rook, e8)
]).

% C) Capturing the checking piece:
% White rook can capture the checking rook.
pcapture_check([
    piece(white, king, e1),
    piece(white, rook, e2),
    piece(black, rook, e8)
]).

% Explainability test positions
pexp_blocked([ piece(white, rook, a1),
               piece(white, knight, a2) ]).

pexp_square_attacked([ piece(white, king, e1),
                       piece(black, rook, e8) ]).

pexp_dest_same_color([ piece(white, knight, b1),
                       piece(white, bishop, c3),
                       piece(white, king, e1) ]).



run_tests :-
    pos1(P1),
    % Test 1: king moving into attacked square should be illegal
    ( ( legal_move(P1, move(white, e1, e2, king)) ) -> fail('king_into_check', false, true) ; ok('king_into_check') ),
    % Also test reason
    ( illegal(P1, move(white, e1, e2, king), Reason1) -> format('Reason for king_into_check: ~w~n', [Reason1]) ; format('No reason returned for king_into_check~n') ),

    % Test 2: rook move that does not expose king should be legal (a1 -> a2)
    ( ( legal_move(P1, move(white, a1, a2, rook)) ) -> ok('rook_simple_move') ; fail('rook_simple_move', true, false) ),

    % Test 3: rook blocked by same-color piece
    pos_blocked(PB),
    ( ( legal_move(PB, move(white, a1, a3, rook)) ) -> fail('rook_blocked', false, true) ; ok('rook_blocked') ),
    ( illegal(PB, move(white, a1, a3, rook), Rb) -> format('Reason for rook_blocked: ~w~n', [Rb]) ; format('No reason for rook_blocked~n') ),

    % Test 4: knight jump to empty legal
    pos_knight_block(PK),
    ( legal_move(PK, move(white, b1, a3, knight)) -> ok('knight_jump') ; fail('knight_jump', true, false) ),

    % Test 5: knight landing on same-color should be illegal
    ( legal_move(PK, move(white, b1, c3, knight)) -> fail('knight_land_same_color', false, true) ; ok('knight_land_same_color') ),
    ( illegal(PK, move(white, b1, c3, knight), Rk) -> format('Reason for knight_land_same_color: ~w~n', [Rk]) ; format('No reason for knight_land_same_color~n') ),

    % Test 6: bishop diagonal capture of enemy piece
    pos_bishop_capture(PB2),
    ( legal_move(PB2, move(white, c1, f4, bishop)) -> ok('bishop_capture') ; fail('bishop_capture', true, false) ),

    % Test 7: any_move_avoids_check on a trivial safe position (should be true)
    ( any_move_avoids_check(P1, white) -> ok('any_move_avoids_check_simple') ; fail('any_move_avoids_check_simple', true, false) ),

    % Test 8: illegal shape example (rook moving diagonally)
    ( illegal(P1, move(white, a1, b2, rook), ReasonR) -> format('Reason for illegal rook shape: ~w~n', [ReasonR]) ; format('No reason for illegal rook shape~n') ),



    % End tests
    format('All tests executed.~n', []).


run_my_tests :-

    % Hours 3–6: Representation & movement understanding
    pempty(Pempty),
    ( path_clear(Pempty, a1, a4) -> ok('path_clear_ok') ;  fail('path_clear_ok', true, false) ),

    pblock(Pblock),
    ( \+ path_clear(Pblock, a1, a4) -> ok('path_clear_blocked') ;  fail('path_clear_blocked', true, false) ),

    pbishop_block(PB),
    ( \+ pseudo_legal_move(PB, move(white, c1, e3, bishop)) -> ok('bishop_blocked') ;  fail('bishop_blocked', false, true) ),

    pknight(PK),
    ( pseudo_legal_move(PK, move(white, b1, a3, knight)) -> ok('knight_jump_ok') ;  fail('knight_jump_ok', true, false) ),


    % ---- Hours 7–10: Attacks and check reasoning ----
    pcheck_rook(P1),
    ( in_check(P1, white) -> ok('check_rook_in_check') ; fail('check_rook_in_check', true, false) ),
    ( attacks(P1, black, e1) -> ok('check_rook_attacks') ; fail('check_rook_attacks', true, false) ),

    pcheck_knight(P2),
    ( in_check(P2, white) -> ok('check_knight_in_check') ; fail('check_knight_in_check', true, false) ),
    ( attacks(P2, black, e1) -> ok('check_knight_attacks') ; fail('check_knight_attacks', true, false) ),

    pcheck_bishop(P3),
    ( in_check(P3, white) -> ok('check_bishop_in_check') ; fail('check_bishop_in_check', true, false) ),
    ( attacks(P3, black, e1) -> ok('check_bishop_attacks') ; fail('check_bishop_attacks', true, false) ),


    % ---- Hours 11–14: legal moves & hypothetical reasoning ----

    % Test 1 — pseudo-legal but illegal due to king exposure

    pexpose_king(Pe),
    ( pseudo_legal_move(Pe, move(white, e2, a2, rook))
      -> ok('exposure_pseudo_legal_ok')
      ;  fail('exposure_pseudo_legal_ok', true, false)
    ),
    ( \+ legal_move(Pe, move(white, e2, a2, rook))
      -> ok('exposure_legal_fails')
      ;  fail('exposure_legal_fails', false, true)
    ),


    % Test 2 — block the check

    pblock_check(Pb),
    ( legal_move(Pb, move(white, e2, e3, rook)) -> ok('block_check_legal') ;  fail('block_check_legal', true, false) ),


    % Test 3 — capture the checking piece

    pcapture_check(Pc),
    ( legal_move(Pc, move(white, e2, e8, rook)) -> ok('capture_check_legal') ;  fail('capture_check_legal', true, false) ),


    % ---- Hours 15–17: Explainability ----

    pexp_blocked(PBExp),
    ( illegal(PBExp, move(white, a1, a3, rook), blocked) -> ok('exp_reason_blocked') ;  ( illegal(PBExp, move(white, a1, a3, rook), R),
           fail('exp_reason_blocked', blocked, R) ; fail('exp_reason_blocked', blocked, no_reason_returned)) ),

    pexp_square_attacked(PSExp),
    ( illegal(PSExp, move(white, e1, e2, king), square_attacked) -> ok('exp_reason_square_attacked') ;  ( illegal(PSExp, move(white, e1, e2, king), R2),
           fail('exp_reason_square_attacked', square_attacked, R2) ; fail('exp_reason_square_attacked', square_attacked, no_reason_returned)) ),

    pexp_dest_same_color(PDExp),
    ( illegal(PDExp, move(white, b1, c3, knight), destination_occupied_by_same_color) -> ok('exp_reason_dest_same_color') ;  ( illegal(PDExp, move(white, b1, c3, knight), R3),
           fail('exp_reason_dest_same_color', destination_occupied_by_same_color, R3) ; fail('exp_reason_dest_same_color', destination_occupied_by_same_color, no_reason_returned))
    ),


    % End tests
    format('All my tests executed.~n', []).

% Allow direct call: ?- run_tests.
