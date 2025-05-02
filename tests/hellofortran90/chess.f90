
! ============================================
! Main Program: Human vs Computer
! ============================================
PROGRAM Fortran_Chess
    USE Chess_Types
    USE Board_Utils
    USE Move_Generation ! Needs Make_Unmake implicitly
    USE Make_Unmake
    USE Search
    IMPLICIT NONE

    TYPE(Board_Type) :: game_board
    TYPE(Move_Type) :: chosen_move
    TYPE(UnmakeInfo_Type) :: move_info ! Needed for make_move call
    LOGICAL :: move_found, is_human_turn, game_over, is_checkmate_flag, is_stalemate_flag
    INTEGER :: human_player_color, ai_player_color, winner
    INTEGER :: search_depth
    CHARACTER(LEN=10) :: user_input
    CHARACTER(LEN=1) :: from_f_char, from_r_char, to_f_char, to_r_char, promo_char
    TYPE(Square_Type) :: parsed_from_sq, parsed_to_sq
    INTEGER :: parsed_promo_piece
    TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
    INTEGER :: num_legal_moves, i

    search_depth = 4 ! AI Difficulty

    ! --- Player Color Selection ---
    DO
        PRINT *, "Choose your color (White/Black): "
        READ *, user_input
        SELECT CASE (TRIM(ADJUSTL(user_input))) ! Basic input handling
        CASE ('White', 'white', 'W', 'w')
            human_player_color = WHITE
            ai_player_color = BLACK
            PRINT *, "You play as White."
            EXIT
        CASE ('Black', 'black', 'B', 'b')
            human_player_color = BLACK
            ai_player_color = WHITE
            PRINT *, "You play as Black."
            EXIT
        CASE DEFAULT
            PRINT *, "Invalid input. Please enter 'White' or 'Black'."
        END SELECT
    END DO

    ! --- Initialize and Print Board ---
    CALL init_board(game_board)
    CALL print_board(game_board)

    ! --- Game Loop ---
    game_over = .FALSE.
    DO WHILE (.NOT. game_over)

        ! 1. Check Game Over
        ! Need a way to check checkmate/stalemate without modifying board state here,
        ! or accept that generate_moves modifies it temporarily.
        CALL generate_moves(game_board, legal_moves, num_legal_moves)
        IF (num_legal_moves == 0) THEN
            IF (is_in_check(game_board, game_board%current_player)) THEN
                winner = get_opponent_color(game_board%current_player)
                IF (winner == WHITE) THEN
                    PRINT *, "=== CHECKMATE! White wins! ==="
                ELSE
                    PRINT *, "=== CHECKMATE! Black wins! ==="
                END IF
            ELSE
                 PRINT *, "=== STALEMATE! Draw. ==="
            END IF
            game_over = .TRUE.
            EXIT ! Exit game loop
        END IF

        ! 2. Determine Turn
        is_human_turn = (game_board%current_player == human_player_color)

        IF (is_human_turn) THEN
            ! --- Human's Turn ---
            PRINT *, " " ! Newline
            PRINT *, "Your turn. Enter move (e.g., e2e4, e7e8q): "
            move_found = .FALSE.
            DO WHILE (.NOT. move_found)
                READ *, user_input
                 IF (TRIM(ADJUSTL(user_input)) == 'quit' .OR. TRIM(ADJUSTL(user_input)) == 'exit') THEN
                    PRINT *, "Exiting game."
                    game_over = .TRUE.
                    EXIT ! Exit inner loop
                 END IF

                ! Basic Parsing (Needs Error Handling!)
                IF (LEN_TRIM(user_input) >= 4) THEN
                    from_f_char = user_input(1:1); from_r_char = user_input(2:2)
                    to_f_char = user_input(3:3);   to_r_char = user_input(4:4)
                    parsed_from_sq%file = char_to_file(from_f_char)
                    parsed_from_sq%rank = char_to_rank(from_r_char)
                    parsed_to_sq%file = char_to_file(to_f_char)
                    parsed_to_sq%rank = char_to_rank(to_r_char)

                    parsed_promo_piece = NO_PIECE
                    IF (LEN_TRIM(user_input) == 5) THEN
                         promo_char = user_input(5:5)
                         SELECT CASE(promo_char)
                         CASE('q'); parsed_promo_piece = QUEEN
                         CASE('r'); parsed_promo_piece = ROOK
                         CASE('b'); parsed_promo_piece = BISHOP
                         CASE('n'); parsed_promo_piece = KNIGHT
                         END SELECT
                    END IF

                    ! Find the move in the legal list
                    DO i = 1, num_legal_moves
                         IF (legal_moves(i)%from_sq%rank == parsed_from_sq%rank .AND. &
                             legal_moves(i)%from_sq%file == parsed_from_sq%file .AND. &
                             legal_moves(i)%to_sq%rank == parsed_to_sq%rank .AND. &
                             legal_moves(i)%to_sq%file == parsed_to_sq%file .AND. &
                             legal_moves(i)%promotion_piece == parsed_promo_piece) THEN
                             chosen_move = legal_moves(i)
                             move_found = .TRUE.
                             EXIT ! Exit move finding loop
                         END IF
                    END DO
                END IF

                IF (.NOT. move_found .AND. .NOT. game_over) THEN
                     PRINT *, "Invalid or illegal move. Try again:"
                END IF
            END DO ! End move input loop

             IF (game_over) EXIT ! Exit game loop if user quit

             ! Make human move
             PRINT *, "You moved." ! Add more detail later
             CALL make_move(game_board, chosen_move, move_info)

        ELSE
            ! --- AI's Turn ---
            PRINT *, " " ! Newline
            PRINT *, "Computer's turn. Thinking..."
            CALL find_best_move(game_board, search_depth, move_found, chosen_move)
            IF (move_found) THEN
                PRINT *, "Computer moved." ! Add move details later
                CALL make_move(game_board, chosen_move, move_info)
            ELSE
                ! Should be caught by game over check, but safety print
                PRINT *, "Error: AI found no move but game not over?"
                game_over = .TRUE.
            END IF
        END IF

        ! Print board after move (if game not over)
         IF (.NOT. game_over) THEN
             CALL print_board(game_board)
         END IF

    END DO ! End game loop

    PRINT *, "Game finished."

END PROGRAM Fortran_Chess
