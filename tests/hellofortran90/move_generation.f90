! ============================================
! Module: Move_Generation
! Purpose: Generate pseudo-legal and legal moves
! ============================================
MODULE Move_Generation
    USE Chess_Types
    USE Board_Utils
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: generate_moves ! Main function exposed

CONTAINS

    ! --- Helper to add a move to the list if array not full ---
    SUBROUTINE add_move(move_list, num_moves, new_move)
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves
        TYPE(Move_Type), INTENT(IN) :: new_move
        IF (num_moves < MAX_MOVES) THEN
            num_moves = num_moves + 1
            move_list(num_moves) = new_move
        ELSE
            PRINT *, "Warning: Move list full!"
            ! Handle error - maybe stop program or ignore move
        END IF
    END SUBROUTINE add_move

    ! --- Generate Pawn Moves ---
    SUBROUTINE generate_pawn_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list ! Array to store moves
        INTEGER, INTENT(INOUT) :: num_moves           ! Current count of moves in list

        INTEGER :: r, f, dir, start_rank, promotion_rank, next_r, dbl_r, target_f
        INTEGER :: player_color, opponent_color, target_color, target_piece
        TYPE(Square_Type) :: to_sq, ep_sq
        TYPE(Move_Type) :: new_move
        LOGICAL :: can_promote
        INTEGER, DIMENSION(4) :: promotion_options = (/ QUEEN, ROOK, BISHOP, KNIGHT /)
        INTEGER :: i

        r = from_sq%rank
        f = from_sq%file
        player_color = board%current_player
        opponent_color = get_opponent_color(player_color)

        IF (player_color == WHITE) THEN
            dir = 1
            start_rank = 2
            promotion_rank = 8
        ELSE
            dir = -1
            start_rank = 7
            promotion_rank = 1
        END IF

        ! 1. Single Push
        next_r = r + dir
        IF (sq_is_valid(next_r, f)) THEN
            IF (board%squares_piece(next_r, f) == NO_PIECE) THEN
                can_promote = (next_r == promotion_rank)
                to_sq = file_rank_to_sq(f, next_r)
                IF (can_promote) THEN
                    DO i = 1, 4
                         new_move%from_sq = from_sq
                         new_move%to_sq = to_sq
                         new_move%promotion_piece = promotion_options(i)
                         new_move%captured_piece = NO_PIECE
                         new_move%is_castling = .FALSE.
                         new_move%is_en_passant = .FALSE.
                         CALL add_move(move_list, num_moves, new_move)
                    END DO
                ELSE
                     new_move%from_sq = from_sq
                     new_move%to_sq = to_sq
                     new_move%promotion_piece = NO_PIECE
                     new_move%captured_piece = NO_PIECE
                     new_move%is_castling = .FALSE.
                     new_move%is_en_passant = .FALSE.
                     CALL add_move(move_list, num_moves, new_move)

                     ! 2. Double Push (only if single push was possible)
                     IF (r == start_rank) THEN
                          dbl_r = r + 2*dir
                          IF (sq_is_valid(dbl_r, f)) THEN
                               IF (board%squares_piece(dbl_r, f) == NO_PIECE) THEN
                                    to_sq = file_rank_to_sq(f, dbl_r)
                                    new_move%from_sq = from_sq
                                    new_move%to_sq = to_sq
                                    new_move%promotion_piece = NO_PIECE
                                    new_move%captured_piece = NO_PIECE
                                    new_move%is_castling = .FALSE.
                                    new_move%is_en_passant = .FALSE.
                                    CALL add_move(move_list, num_moves, new_move)
                               END IF
                          END IF
                     END IF
                END IF
            END IF
        END IF

        ! 3. Captures (Diagonal)
        IF (sq_is_valid(next_r, 1)) THEN ! Only need to check rank validity once
             DO target_f = f-1, f+1, 2 ! Check f-1 and f+1
                  IF (sq_is_valid(next_r, target_f)) THEN
                       target_piece = board%squares_piece(next_r, target_f)
                       target_color = board%squares_color(next_r, target_f)
                       to_sq = file_rank_to_sq(target_f, next_r)

                       ! Regular Capture
                       IF (target_piece /= NO_PIECE .AND. target_color == opponent_color) THEN
                            can_promote = (next_r == promotion_rank)
                            IF (can_promote) THEN
                                 DO i = 1, 4
                                     new_move%from_sq = from_sq
                                     new_move%to_sq = to_sq
                                     new_move%promotion_piece = promotion_options(i)
                                     new_move%captured_piece = target_piece
                                     new_move%is_castling = .FALSE.
                                     new_move%is_en_passant = .FALSE.
                                     CALL add_move(move_list, num_moves, new_move)
                                 END DO
                            ELSE
                                 new_move%from_sq = from_sq
                                 new_move%to_sq = to_sq
                                 new_move%promotion_piece = NO_PIECE
                                 new_move%captured_piece = target_piece
                                 new_move%is_castling = .FALSE.
                                 new_move%is_en_passant = .FALSE.
                                 CALL add_move(move_list, num_moves, new_move)
                            END IF
                       ! En Passant Capture
                       ELSE IF (board%ep_target_present .AND. &
                                next_r == board%ep_target_sq%rank .AND. &
                                target_f == board%ep_target_sq%file) THEN
                            new_move%from_sq = from_sq
                            new_move%to_sq = board%ep_target_sq ! Move to EP target square
                            new_move%promotion_piece = NO_PIECE
                            new_move%captured_piece = PAWN ! EP always captures a pawn
                            new_move%is_castling = .FALSE.
                            new_move%is_en_passant = .TRUE.
                            CALL add_move(move_list, num_moves, new_move)
                       END IF
                  END IF
             END DO
        END IF

    END SUBROUTINE generate_pawn_moves

    ! --- Generate Knight Moves ---
    SUBROUTINE generate_knight_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves

        INTEGER :: r, f, nr, nf, target_piece, target_color
        INTEGER, DIMENSION(8,2) :: deltas
        TYPE(Square_Type) :: to_sq
        TYPE(Move_Type) :: new_move
        INTEGER :: i

        r = from_sq%rank
        f = from_sq%file

        deltas = RESHAPE((/ 2, 1, -1, -2, -2, -1, 1, 2, &
             1, 2,  2,  1, -1, -2, -2, -1 /), (/8, 2/))
        

        new_move%from_sq = from_sq
        new_move%promotion_piece = NO_PIECE
        new_move%is_castling = .FALSE.
        new_move%is_en_passant = .FALSE.

        DO i = 1, 8
            nr = r + deltas(i, 1)
            nf = f + deltas(i, 2)
            IF (sq_is_valid(nr, nf)) THEN
                target_piece = board%squares_piece(nr, nf)
                target_color = board%squares_color(nr, nf)
                to_sq = file_rank_to_sq(nf, nr)
                new_move%to_sq = to_sq

                IF (target_piece == NO_PIECE) THEN ! Move to empty square
                    new_move%captured_piece = NO_PIECE
                    CALL add_move(move_list, num_moves, new_move)
                ELSE IF (target_color /= board%current_player) THEN ! Capture
                    new_move%captured_piece = target_piece
                    CALL add_move(move_list, num_moves, new_move)
                END IF
            END IF
        END DO
    END SUBROUTINE generate_knight_moves


    ! --- Generate Sliding Moves (Rook, Bishop, Queen) ---
     SUBROUTINE generate_sliding_moves(board, from_sq, directions, num_dirs, move_list, num_moves)
         TYPE(Board_Type), INTENT(IN) :: board
         TYPE(Square_Type), INTENT(IN) :: from_sq
         INTEGER, DIMENSION(num_dirs, 2), INTENT(IN) :: directions
         INTEGER, INTENT(IN) :: num_dirs
         TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
         INTEGER, INTENT(INOUT) :: num_moves

         INTEGER :: r, f, nr, nf, target_piece, target_color, dr, df
         TYPE(Square_Type) :: to_sq
         TYPE(Move_Type) :: new_move
         INTEGER :: i

         r = from_sq%rank
         f = from_sq%file

         new_move%from_sq = from_sq
         new_move%promotion_piece = NO_PIECE
         new_move%is_castling = .FALSE.
         new_move%is_en_passant = .FALSE.

         DO i = 1, num_dirs
             dr = directions(i, 1)
             df = directions(i, 2)
             nr = r + dr
             nf = f + df
             DO WHILE (sq_is_valid(nr, nf))
                 target_piece = board%squares_piece(nr, nf)
                 target_color = board%squares_color(nr, nf)
                 to_sq = file_rank_to_sq(nf, nr)
                 new_move%to_sq = to_sq

                 IF (target_piece == NO_PIECE) THEN ! Move to empty square
                     new_move%captured_piece = NO_PIECE
                     CALL add_move(move_list, num_moves, new_move)
                 ELSE ! Hit a piece
                     IF (target_color /= board%current_player) THEN ! Capture
                         new_move%captured_piece = target_piece
                         CALL add_move(move_list, num_moves, new_move)
                     END IF
                     EXIT ! Stop searching this direction (path blocked)
                 END IF

                 nr = nr + dr ! Continue sliding
                 nf = nf + df
             END DO
         END DO
     END SUBROUTINE generate_sliding_moves

     ! --- Generate King Moves (Including Castling placeholders) ---
     SUBROUTINE generate_king_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves

        INTEGER :: r, f, nr, nf, target_piece, target_color, back_rank
        INTEGER, DIMENSION(8,2) :: deltas
        TYPE(Square_Type) :: to_sq
        TYPE(Move_Type) :: new_move
        INTEGER :: i
        LOGICAL :: can_castle_k, can_castle_q

        r = from_sq%rank
        f = from_sq%file

        ! 1. Normal Moves
        deltas = RESHAPE((/  1,  0, -1,  0,  1,  1, -1, -1, &
                          0,  1,  0, -1, 1, -1,  1, -1 /), (/8, 2/))

        new_move%from_sq = from_sq
        new_move%promotion_piece = NO_PIECE
        new_move%is_castling = .FALSE.
        new_move%is_en_passant = .FALSE.

        DO i = 1, 8
            nr = r + deltas(i, 1)
            nf = f + deltas(i, 2)
            IF (sq_is_valid(nr, nf)) THEN
                target_piece = board%squares_piece(nr, nf)
                target_color = board%squares_color(nr, nf)
                to_sq = file_rank_to_sq(nf, nr)
                new_move%to_sq = to_sq

                IF (target_piece == NO_PIECE) THEN ! Move to empty square
                    new_move%captured_piece = NO_PIECE
                    CALL add_move(move_list, num_moves, new_move)
                ELSE IF (target_color /= board%current_player) THEN ! Capture
                    new_move%captured_piece = target_piece
                    CALL add_move(move_list, num_moves, new_move)
                END IF
            END IF
        END DO

        ! 2. Castling (Pseudo-legal check: rights and empty squares)
        !    Legality check (squares not attacked) happens in generate_moves
        IF (board%current_player == WHITE) THEN
             back_rank = 1
             can_castle_k = board%wc_k
             can_castle_q = board%wc_q
        ELSE
             back_rank = 8
             can_castle_k = board%bc_k
             can_castle_q = board%bc_q
        END IF

        IF (r == back_rank .AND. f == 5) THEN ! King on e1/e8
             ! Kingside
             IF (can_castle_k .AND. &
                 board%squares_piece(back_rank, 6) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 7) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 8) == ROOK .AND. & ! Check rook presence
                 board%squares_color(back_rank, 8) == board%current_player) THEN

                 new_move%from_sq = from_sq
                 new_move%to_sq = file_rank_to_sq(7, back_rank) ! King to g1/g8
                 new_move%promotion_piece = NO_PIECE
                 new_move%captured_piece = NO_PIECE
                 new_move%is_castling = .TRUE.
                 new_move%is_en_passant = .FALSE.
                 CALL add_move(move_list, num_moves, new_move)
             END IF
             ! Queenside
             IF (can_castle_q .AND. &
                 board%squares_piece(back_rank, 4) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 3) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 2) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 1) == ROOK .AND. & ! Check rook presence
                 board%squares_color(back_rank, 1) == board%current_player) THEN

                 new_move%from_sq = from_sq
                 new_move%to_sq = file_rank_to_sq(3, back_rank) ! King to c1/c8
                 new_move%promotion_piece = NO_PIECE
                 new_move%captured_piece = NO_PIECE
                 new_move%is_castling = .TRUE.
                 new_move%is_en_passant = .FALSE.
                 CALL add_move(move_list, num_moves, new_move)
             END IF
        END IF

     END SUBROUTINE generate_king_moves


    ! --- Generate All Pseudo-Legal Moves ---
    SUBROUTINE generate_pseudo_moves(board, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list ! Array to store moves
        INTEGER, INTENT(OUT) :: num_moves           ! Count of moves generated

        INTEGER :: r, f, piece, color
        TYPE(Square_Type) :: from_sq
        ! Define directions for sliding pieces here
        INTEGER, DIMENSION(4, 2) :: bishop_dirs = RESHAPE((/  1,  1,  1, -1, -1,  1, -1, -1 /), (/4, 2/))
        INTEGER, DIMENSION(4, 2) :: rook_dirs = RESHAPE((/  1,  0, -1,  0,  0,  1,  0, -1 /), (/4, 2/))
        INTEGER, DIMENSION(8, 2) :: queen_dirs = RESHAPE([ 1, -1,  0,  0,  1,  1, -1, -1, &
                                                   0,  0,  1, -1, 1, -1,  1, -1 ], [8, 2])

        num_moves = 0 ! Reset count

        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                piece = board%squares_piece(r, f)
                color = board%squares_color(r, f)

                IF (color == board%current_player) THEN
                    from_sq = file_rank_to_sq(f, r)
                    SELECT CASE (piece)
                    CASE (PAWN)
                        CALL generate_pawn_moves(board, from_sq, move_list, num_moves)
                    CASE (KNIGHT)
                        CALL generate_knight_moves(board, from_sq, move_list, num_moves)
                    CASE (BISHOP)
                        CALL generate_sliding_moves(board, from_sq, bishop_dirs, 4, move_list, num_moves)
                    CASE (ROOK)
                        CALL generate_sliding_moves(board, from_sq, rook_dirs, 4, move_list, num_moves)
                    CASE (QUEEN)
                        CALL generate_sliding_moves(board, from_sq, queen_dirs, 8, move_list, num_moves)
                    CASE (KING)
                        CALL generate_king_moves(board, from_sq, move_list, num_moves)
                    END SELECT
                END IF
            END DO
        END DO
    END SUBROUTINE generate_pseudo_moves


    ! --- Generate Legal Moves (Filters Pseudo-Legal) ---
    SUBROUTINE generate_moves(board, legal_move_list, num_legal_moves)
        USE make_unmake
        TYPE(Board_Type), INTENT(INOUT) :: board ! Needs INOUT for make/unmake
        TYPE(Move_Type), DIMENSION(:), INTENT(OUT) :: legal_move_list
        INTEGER, INTENT(OUT) :: num_legal_moves

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: pseudo_moves
        INTEGER :: num_pseudo_moves
        INTEGER :: i
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: is_legal
        INTEGER :: player_color, opponent_color
        TYPE(Square_Type) :: king_sq, mid_sq

        num_legal_moves = 0
        player_color = board%current_player
        opponent_color = get_opponent_color(player_color)

        ! 1. Generate all pseudo-legal moves
        CALL generate_pseudo_moves(board, pseudo_moves, num_pseudo_moves)

        ! 2. Filter for legality
        DO i = 1, num_pseudo_moves
            current_move = pseudo_moves(i)
            is_legal = .TRUE. ! Assume legal initially

            ! Special check for castling through check
            IF (current_move%is_castling) THEN
                 king_sq = current_move%from_sq
                 IF (current_move%to_sq%file == 7) THEN ! Kingside
                     mid_sq = file_rank_to_sq(6, king_sq%rank)
                 ELSE ! Queenside (to_sq%file == 3)
                     mid_sq = file_rank_to_sq(4, king_sq%rank)
                 END IF
                 IF (is_square_attacked(board, king_sq, opponent_color) .OR. &
                     is_square_attacked(board, mid_sq, opponent_color) .OR. &
                     is_square_attacked(board, current_move%to_sq, opponent_color)) THEN
                     is_legal = .FALSE.
                 END IF
            END IF

            IF (is_legal) THEN
                 ! 3. Make the move, check if king is safe, unmake the move
                 CALL make_move(board, current_move, unmake_info)
                 IF (.NOT. is_in_check(board, player_color)) THEN
                     ! Add to legal move list
                     CALL add_move(legal_move_list, num_legal_moves, current_move)
                 END IF
                 CALL unmake_move(board, current_move, unmake_info)
            END IF
        END DO

    END SUBROUTINE generate_moves


  END MODULE Move_Generation
