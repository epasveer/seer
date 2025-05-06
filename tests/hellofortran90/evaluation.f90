! ============================================
! Module: Evaluation
! Purpose: Static board evaluation
! ============================================
MODULE Evaluation
    USE Chess_Types
    USE Board_Utils
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: evaluate_board


INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PST = RESHAPE( &
    [ 0, 50, 10, 5, 0, 5, 5, 0, &  ! Column 1
      0, 50, 10, 5, 0, -5, 10, 0, &  ! Column 2
      0, 50, 20, 10, 0, -10, 10, 0, &  ! Column 3
      0, 50, 30, 25, 20, 0, -20, 0, &  ! Column 4
      0, 50, 30, 25, 20, -10, -20, 0, &  ! Column 5
      0, 50, 20, 10, 0, -10, 10, 0, &  ! Column 6
      0, 50, 10, 5, 0, 5, 10, 0, &  ! Column 7
      0, 50, 0, 0, 0, 0, 0, 0 ], &  ! Column 8
    SHAPE(PAWN_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PST = RESHAPE( &
    [ -50, -40, -30, -30, -30, -30, -40, -50, &  ! Column 1
      -40, -20, 0, 0, 0, 0, -20, -40, &  ! Column 2
      -30, 0, 10, 15, 15, 10, 0, -30, &  ! Column 3
      -30, 5, 15, 20, 20, 15, 5, -30, &  ! Column 4
      -30, 0, 15, 20, 20, 15, 0, -30, &  ! Column 5
      -30, 5, 10, 15, 15, 10, 5, -30, &  ! Column 6
      -40, -20, 0, 5, 5, 0, -20, -40, &  ! Column 7
      -50, -40, -30, -30, -30, -30, -40, -50 ], &  ! Column 8
    SHAPE(KNIGHT_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PST = RESHAPE( &
    [ -20, -10, -10, -10, -10, -10, -10, -20, &  ! Column 1
      -10, 0, 0, 0, 0, 0, 0, -10, &  ! Column 2
      -10, 0, 10, 10, 10, 10, 0, -10, &  ! Column 3
      -10, 5, 5, 10, 10, 5, 5, -10, &  ! Column 4
      -10, 0, 5, 10, 10, 5, 0, -10, &  ! Column 5
      -10, 5, 5, 5, 5, 5, 5, -10, &  ! Column 6
      -10, 0, 5, 0, 0, 5, 0, -10, &  ! Column 7
      -20, -10, -10, -10, -10, -10, -10, -20 ], &  ! Column 8
    SHAPE(BISHOP_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PST = RESHAPE( &
    [ 0, 0, 0, 0, 0, 0, 0, 0, &  ! Column 1
      5, 10, 10, 10, 10, 10, 10, 5, &  ! Column 2
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 3
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 4
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 5
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 6
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 7
      0, 0, 0, 5, 5, 0, 0, 0 ], &  ! Column 8
    SHAPE(ROOK_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PST = RESHAPE( &
    [ -20, -10, -10, -5, -5, -10, -10, -20, &  ! Column 1
      -10, 0, 0, 0, 0, 0, 0, -10, &  ! Column 2
      -10, 0, 5, 5, 5, 5, 0, -10, &  ! Column 3
      -5, 0, 5, 5, 5, 5, 0, -5, &  ! Column 4
      0, 0, 5, 5, 5, 5, 0, -5, &  ! Column 5
      -10, 5, 5, 5, 5, 5, 0, -10, &  ! Column 6
      -10, 0, 5, 0, 0, 0, 0, -10, &  ! Column 7
      -20, -10, -10, -5, -5, -10, -10, -20 ], &  ! Column 8
    SHAPE(QUEEN_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PST = RESHAPE( &
    [ -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 1
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 2
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 3
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 4
      -20, -30, -30, -40, -40, -30, -30, -20, &  ! Column 5
      -10, -20, -20, -20, -20, -20, -20, -10, &  ! Column 6
      20, 20, 0, 0, 0, 0, 20, 20, &  ! Column 7
      20, 30, 10, 0, 0, 10, 30, 20 ], &  ! Column 8
    SHAPE(KING_PST))

     INTEGER, PARAMETER :: PAWN_VAL = 100, KNIGHT_VAL = 320, BISHOP_VAL = 330, &
                           ROOK_VAL = 500, QUEEN_VAL = 900, KING_VAL = 20000

CONTAINS
    ! --- Evaluate Board ---
    INTEGER FUNCTION evaluate_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: score, r, f, piece, color, eval_rank, piece_value, pst_value

        score = 0
        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                piece = board%squares_piece(r, f)
                color = board%squares_color(r, f)
                IF (piece /= NO_PIECE) THEN
                    ! Material value
                    SELECT CASE(piece)
                    CASE(PAWN)   ; piece_value = PAWN_VAL
                    CASE(KNIGHT) ; piece_value = KNIGHT_VAL
                    CASE(BISHOP) ; piece_value = BISHOP_VAL
                    CASE(ROOK)   ; piece_value = ROOK_VAL
                    CASE(QUEEN)  ; piece_value = QUEEN_VAL
                    CASE(KING)   ; piece_value = KING_VAL
                    END SELECT

                    ! Piece Square Table value (needs full tables defined)
                    IF (color == WHITE) THEN
                         eval_rank = r
                    ELSE
                         eval_rank = BOARD_SIZE - r + 1 ! Flip rank for black
                    END IF

                    SELECT CASE(piece)
                    CASE(PAWN)   ; pst_value = PAWN_PST(eval_rank, f)
                    CASE(KNIGHT) ; pst_value = KNIGHT_PST(eval_rank, f)
                    CASE(BISHOP) ; pst_value = BISHOP_PST(eval_rank, f)
                    CASE(ROOK)   ; pst_value = ROOK_PST(eval_rank, f)
                    CASE(QUEEN)  ; pst_value = QUEEN_PST(eval_rank, f)
                    CASE(KING)   ; pst_value = KING_PST(eval_rank, f)
                    ! Add cases for other pieces using their PSTs
                    CASE DEFAULT ; pst_value = 0
                    END SELECT

                    IF (color == WHITE) THEN
                        score = score + piece_value + pst_value
                    ELSE
                        score = score - (piece_value + pst_value)
                    END IF
                END IF
            END DO
        END DO

        ! Return score relative to current player
        IF (board%current_player == WHITE) THEN
            evaluate_board = score
        ELSE
            evaluate_board = -score
        END IF

    END FUNCTION evaluate_board

END MODULE Evaluation
