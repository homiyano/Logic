
% ---------------------------------------------------------------------
% tests.pl
% Simple test harness for chess.pl starter file.
% Loads chess.pl and runs a series of checks printing expected vs. actual.
% ---------------------------------------------------------------------

:- ensure_loaded('chess.pl').

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

% Allow direct call: ?- run_tests.
