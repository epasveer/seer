! ============================================
! Module: Search
! Purpose: AI search algorithm
! ============================================
MODULE Search
    USE Chess_Types
    USE Board_Utils
    USE Move_Generation
    USE Make_Unmake
    USE Evaluation
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: find_best_move

    INTEGER, PARAMETER :: MATE_SCORE = 100000 ! A score indicating checkmate
    INTEGER, PARAMETER :: INF = MATE_SCORE + 1000 ! Represents infinity

CONTAINS

    ! --- Negamax Search (Recursive Helper) ---
    RECURSIVE INTEGER FUNCTION negamax(board, depth, alpha, beta) RESULT(best_score)
        TYPE(Board_Type), INTENT(INOUT) :: board ! Needs INOUT for make/unmake
        INTEGER, INTENT(IN) :: depth, alpha, beta
        INTEGER :: score, current_alpha
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check

        current_alpha = alpha ! Local copy to modify

        ! 1. Depth check
        IF (depth <= 0) THEN
            best_score = evaluate_board(board)
            RETURN
        END IF

        ! 2. Generate moves
        CALL generate_moves(board, moves, num_moves)

        ! 3. Checkmate / Stalemate check
        IF (num_moves == 0) THEN
             in_check = is_in_check(board, board%current_player)
             IF (in_check) THEN
                 best_score = -MATE_SCORE + (10 - depth) ! Lose faster if deeper mate
             ELSE
                 best_score = 0 ! Stalemate
             END IF
             RETURN
        END IF

        ! 4. Iterate through moves
        best_score = -INF ! Initialize with worst score

        DO i = 1, num_moves
            current_move = moves(i)
            CALL make_move(board, current_move, unmake_info)
            score = -negamax(board, depth - 1, -beta, -current_alpha) ! Recursive call
            CALL unmake_move(board, current_move, unmake_info)

            ! Update best score and alpha
            IF (score > best_score) THEN
                 best_score = score
            END IF
            IF (best_score > current_alpha) THEN
                 current_alpha = best_score
            END IF

            ! Beta cutoff
            IF (current_alpha >= beta) THEN
                 EXIT ! Prune remaining moves
            END IF
        END DO

    END FUNCTION negamax


    ! --- Find Best Move (Top Level Search Call) ---
    SUBROUTINE find_best_move(board, depth, best_move_found, best_move)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: depth
        LOGICAL, INTENT(OUT) :: best_move_found
        TYPE(Move_Type), INTENT(OUT) :: best_move

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i
        INTEGER :: score, best_score_so_far, alpha, beta
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info

        best_move_found = .FALSE.
        best_score_so_far = -INF
        alpha = -INF
        beta = INF

        CALL generate_moves(board, moves, num_moves)

        IF (num_moves == 0) THEN
             RETURN ! No legal moves
        END IF

        best_move = moves(1) ! Default to first move
        best_move_found = .TRUE.

        DO i = 1, num_moves
            current_move = moves(i)
            CALL make_move(board, current_move, unmake_info)
            score = -negamax(board, depth - 1, -beta, -alpha)
            CALL unmake_move(board, current_move, unmake_info)

            IF (score > best_score_so_far) THEN
                 best_score_so_far = score
                 best_move = current_move
            END IF

            ! Update alpha (for root node, mainly tracking best score)
            IF (best_score_so_far > alpha) THEN
                 alpha = best_score_so_far
            END IF
            ! No beta cutoff at root usually, we want the actual best move
        END DO

    END SUBROUTINE find_best_move

END MODULE Search
