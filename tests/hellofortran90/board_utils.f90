! ============================================
! Module: Board_Utils
! Purpose: Helper functions for board ops
! ============================================
MODULE Board_Utils
    USE Chess_Types
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: init_board, print_board, get_opponent_color, &
              sq_is_valid, char_to_file, char_to_rank, &
              file_rank_to_sq, is_square_attacked, &
              find_king, is_in_check

CONTAINS

    ! --- Function to convert file char ('a'-'h') to index (1-8) ---
    INTEGER FUNCTION char_to_file(file_char)
        CHARACTER(LEN=1), INTENT(IN) :: file_char
        char_to_file = ICHAR(file_char) - ICHAR('a') + 1
    END FUNCTION char_to_file

    ! --- Function to convert rank char ('1'-'8') to index (1-8) ---
    INTEGER FUNCTION char_to_rank(rank_char)
        CHARACTER(LEN=1), INTENT(IN) :: rank_char
        INTEGER :: ierr
        READ (rank_char, '(I1)', IOSTAT=ierr) char_to_rank
        ! Basic error check could be added
    END FUNCTION char_to_rank

    ! --- Subroutine to create a Square_Type ---
    FUNCTION file_rank_to_sq(file, rank) RESULT(sq)
        INTEGER, INTENT(IN) :: file, rank
        TYPE(Square_Type)   :: sq
        sq%file = file
        sq%rank = rank
    END FUNCTION file_rank_to_sq

    ! --- Function to check if square indices are valid (1-8) ---
    LOGICAL FUNCTION sq_is_valid(rank, file)
        INTEGER, INTENT(IN) :: rank, file
        sq_is_valid = (rank >= 1 .AND. rank <= BOARD_SIZE .AND. &
                       file >= 1 .AND. file <= BOARD_SIZE)
    END FUNCTION sq_is_valid

    ! --- Get opponent color ---
    INTEGER FUNCTION get_opponent_color(player_color)
        INTEGER, INTENT(IN) :: player_color
        IF (player_color == WHITE) THEN
            get_opponent_color = BLACK
        ELSE IF (player_color == BLACK) THEN
            get_opponent_color = WHITE
        ELSE
            get_opponent_color = NO_COLOR ! Should not happen
        END IF
    END FUNCTION get_opponent_color

    ! --- Initialize Board to Starting Position ---
    SUBROUTINE init_board(board)
        TYPE(Board_Type), INTENT(OUT) :: board

        INTEGER :: i, f
        INTEGER, DIMENSION(BOARD_SIZE) :: back_rank_pieces = &
                (/ ROOK, KNIGHT, BISHOP, QUEEN, KING, BISHOP, KNIGHT, ROOK /)

        ! Clear board
        board%squares_piece = NO_PIECE
        board%squares_color = NO_COLOR

        ! Place pieces
        DO f = 1, BOARD_SIZE
            ! White pieces
            board%squares_piece(1, f) = back_rank_pieces(f)
            board%squares_color(1, f) = WHITE
            board%squares_piece(2, f) = PAWN
            board%squares_color(2, f) = WHITE
            ! Black pieces
            board%squares_piece(8, f) = back_rank_pieces(f)
            board%squares_color(8, f) = BLACK
            board%squares_piece(7, f) = PAWN
            board%squares_color(7, f) = BLACK
        END DO

        ! Set initial state
        board%current_player = WHITE
        board%ep_target_present = .FALSE.
        board%ep_target_sq%rank = 0
        board%ep_target_sq%file = 0
        board%wc_k = .TRUE.
        board%wc_q = .TRUE.
        board%bc_k = .TRUE.
        board%bc_q = .TRUE.

    END SUBROUTINE init_board

    ! --- Print Board to Console ---
    SUBROUTINE print_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: r, f
        CHARACTER(LEN=1) :: piece_char

        PRINT *, " +---+---+---+---+---+---+---+---+"
        DO r = BOARD_SIZE, 1, -1
            WRITE(*, '(I1,A)', ADVANCE='NO') r, " |"
            DO f = 1, BOARD_SIZE
                SELECT CASE (board%squares_piece(r,f))
                CASE (PAWN)
                    piece_char = 'P'
                CASE (KNIGHT)
                    piece_char = 'N'
                CASE (BISHOP)
                    piece_char = 'B'
                CASE (ROOK)
                    piece_char = 'R'
                CASE (QUEEN)
                    piece_char = 'Q'
                CASE (KING)
                    piece_char = 'K'
                CASE DEFAULT
                    piece_char = ' '
                END SELECT

                IF (board%squares_color(r,f) == BLACK .AND. piece_char /= '.') THEN
                    ! Crude lowercase for black
                    piece_char = ACHAR(IACHAR(piece_char) + 32)
                END IF
                 WRITE(*, '(A,A)', ADVANCE='NO') " "//piece_char//" |"
            END DO
            PRINT *
            PRINT *, " +---+---+---+---+---+---+---+---+"
        END DO
        PRINT *, "    a   b   c   d   e   f   g   h"
        IF (board%current_player == WHITE) THEN
            PRINT *, "Turn: White"
        ELSE
            PRINT *, "Turn: Black"
        END IF
        ! Add EP target, Castling rights printout if desired
    END SUBROUTINE print_board


    ! --- Find King of a given color ---
    FUNCTION find_king(board, king_color) RESULT(king_sq)

        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN)          :: king_color
        TYPE(Square_Type)            :: king_sq
        INTEGER :: r, f

        king_sq%rank = 0 ! Indicate not found initially
        king_sq%file = 0

        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                IF (board%squares_piece(r, f) == KING .AND. &
                    board%squares_color(r, f) == king_color) THEN
                    king_sq%rank = r
                    king_sq%file = f
                    RETURN
                END IF
            END DO

        END DO

    END FUNCTION find_king

    LOGICAL FUNCTION is_square_attacked(board, target_sq, attacker_color)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: target_sq
        INTEGER, INTENT(IN)           :: attacker_color

        INTEGER :: i, tr, tf, r, f, piece, color, dir, df, dr
        INTEGER, DIMENSION(8,2) :: knight_deltas, king_deltas, sliding_deltas
        LOGICAL :: is_diagonal

        is_square_attacked = .FALSE. ! Assume not attacked initially
        tr = target_sq%rank
        tf = target_sq%file

        ! 1. Check Pawn attacks
        IF (attacker_color == WHITE) THEN
            dir = -1 ! White pawns attack southwards (from rank+1)
        ELSE
            dir = 1  ! Black pawns attack northwards (from rank-1)
        END IF
        r = tr + dir
        DO df = -1, 1, 2 ! Check files tf-1 and tf+1
            f = tf + df
            IF (sq_is_valid(r, f)) THEN
                IF (board%squares_piece(r, f) == PAWN .AND. &
                    board%squares_color(r, f) == attacker_color) THEN
                    is_square_attacked = .TRUE.
                    RETURN
                END IF
            END IF
        END DO

        knight_deltas = RESHAPE((/ 2, 1, -1, -2, -2, -1, 1, 2, &
                          1, 2,  2,  1, -1, -2, -2, -1 /), (/8, 2/))

        DO i = 1, 8
             dr = knight_deltas(i, 1)
             df = knight_deltas(i, 2)
             r = tr + dr
             f = tf + df
             IF (sq_is_valid(r,f)) THEN
                  IF (board%squares_piece(r,f) == KNIGHT .AND. &
                      board%squares_color(r,f) == attacker_color) THEN
                      is_square_attacked = .TRUE.
                      RETURN
                  END IF
             END IF
        END DO

        ! 3. Check King attacks
        king_deltas = RESHAPE((/  1,  0, -1,  0,  1,  1, -1, -1, &
                          0,  1,  0, -1, 1, -1,  1, -1 /), (/8, 2/))

        DO i = 1, 8
             dr = king_deltas(i, 1)
             df = king_deltas(i, 2)
             r = tr + dr
             f = tf + df
             IF (sq_is_valid(r,f)) THEN
                  IF (board%squares_piece(r,f) == KING .AND. &
                      board%squares_color(r,f) == attacker_color) THEN
                      is_square_attacked = .TRUE.
                      RETURN
                  END IF
             END IF
        END DO

        ! 4. Check Sliding attacks (Rook, Bishop, Queen)
        sliding_deltas = RESHAPE((/ 1, -1, 0,  0,  1,  1, -1, -1, &
                           0,  0, 1, -1,  1, -1,  1, -1 /), (/8, 2/))

        DO i = 1, 8
            dr = sliding_deltas(i, 1)
            df = sliding_deltas(i, 2)
            is_diagonal = (i > 4)
            r = tr + dr
            f = tf + df
            DO WHILE (sq_is_valid(r, f))
                piece = board%squares_piece(r,f)
                color = board%squares_color(r,f)
                IF (piece /= NO_PIECE) THEN ! Found a piece
                    IF (color == attacker_color) THEN
                        SELECT CASE(piece)
                        CASE(QUEEN)
                            is_square_attacked = .TRUE.; RETURN
                        CASE(ROOK)
                            IF (.NOT. is_diagonal) THEN
                                is_square_attacked = .TRUE.; RETURN
                            END IF
                        CASE(BISHOP)
                            IF (is_diagonal) THEN
                                is_square_attacked = .TRUE.; RETURN
                            END IF
                        END SELECT
                    END IF
                    EXIT ! Path blocked, stop searching this direction
                END IF
                r = r + dr
                f = f + df
            END DO
        END DO

    END FUNCTION is_square_attacked

    ! --- Check if the king of 'player_color' is in check ---
    LOGICAL FUNCTION is_in_check(board, player_color)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN)          :: player_color
        TYPE(Square_Type) :: king_sq
        INTEGER           :: attacker_color

        king_sq = find_king(board, player_color)
        IF (king_sq%rank == 0) THEN ! King not found (error state)
             is_in_check = .FALSE. ! Or handle error
             RETURN
        END IF

        attacker_color = get_opponent_color(player_color)
        is_in_check = is_square_attacked(board, king_sq, attacker_color)

    END FUNCTION is_in_check


END MODULE Board_Utils
