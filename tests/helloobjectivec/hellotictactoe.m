#import <Foundation/Foundation.h>
#import <stdio.h>

@interface TicTacToeGame : NSObject {
    char board[3][3];
    char currentPlayer;
    BOOL gameWon;
    BOOL gameDraw;
}

- (id)init;
- (void)initializeBoard;
- (void)displayBoard;
- (void)makeMove:(int)position;
- (BOOL)isValidMove:(int)position;
- (BOOL)checkWin;
- (BOOL)checkDraw;
- (void)switchPlayer;
- (void)playGame;
- (int)getPlayerInput;

@end

@implementation TicTacToeGame

- (id)init {
    self = [super init];
    if (self) {
        currentPlayer = 'X';
        gameWon = NO;
        gameDraw = NO;
        [self initializeBoard];
    }
    return self;
}

- (void)initializeBoard {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            board[i][j] = ' ';
        }
    }
}

- (void)displayBoard {
    printf("\n");
    printf("   |   |   \n");
    printf(" %c | %c | %c \n", board[0][0], board[0][1], board[0][2]);
    printf("___|___|___\n");
    printf("   |   |   \n");
    printf(" %c | %c | %c \n", board[1][0], board[1][1], board[1][2]);
    printf("___|___|___\n");
    printf("   |   |   \n");
    printf(" %c | %c | %c \n", board[2][0], board[2][1], board[2][2]);
    printf("   |   |   \n\n");
    
    printf("Positions:\n");
    printf(" 1 | 2 | 3 \n");
    printf("___|___|___\n");
    printf(" 4 | 5 | 6 \n");
    printf("___|___|___\n");
    printf(" 7 | 8 | 9 \n\n");
}

- (BOOL)isValidMove:(int)position {
    if (position < 1 || position > 9) {
        return NO;
    }
    
    int row = (position - 1) / 3;
    int col = (position - 1) % 3;
    
    return board[row][col] == ' ';
}

- (void)makeMove:(int)position {
    int row = (position - 1) / 3;
    int col = (position - 1) % 3;
    
    board[row][col] = currentPlayer;
}

- (BOOL)checkWin {
    // Check rows
    for (int i = 0; i < 3; i++) {
        if (board[i][0] == currentPlayer && 
            board[i][1] == currentPlayer && 
            board[i][2] == currentPlayer) {
            return YES;
        }
    }
    
    // Check columns
    for (int j = 0; j < 3; j++) {
        if (board[0][j] == currentPlayer && 
            board[1][j] == currentPlayer && 
            board[2][j] == currentPlayer) {
            return YES;
        }
    }
    
    // Check diagonals
    if ((board[0][0] == currentPlayer && 
         board[1][1] == currentPlayer && 
         board[2][2] == currentPlayer) ||
        (board[0][2] == currentPlayer && 
         board[1][1] == currentPlayer && 
         board[2][0] == currentPlayer)) {
        return YES;
    }
    
    return NO;
}

- (BOOL)checkDraw {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (board[i][j] == ' ') {
                return NO;
            }
        }
    }
    return YES;
}

- (void)switchPlayer {
    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
}

- (int)getPlayerInput {
    int input;
    printf("Player %c, enter your move (1-9): ", currentPlayer);
    (void)scanf("%d", &input);
    
    // Clear input buffer
    int c;
    while ((c = getchar()) != '\n' && c != EOF);
    
    return input;
}

- (void)playGame {
    printf("Welcome to Tic Tac Toe!\n");
    printf("Player X goes first.\n");
    
    while (!gameWon && !gameDraw) {
        [self displayBoard];
        
        int move = [self getPlayerInput];
        
        if ([self isValidMove:move]) {
            [self makeMove:move];
            
            if ([self checkWin]) {
                gameWon = YES;
                [self displayBoard];
                printf("🎉 Player %c wins! 🎉\n", currentPlayer);
            } else if ([self checkDraw]) {
                gameDraw = YES;
                [self displayBoard];
                printf("It's a draw! Good game!\n");
            } else {
                [self switchPlayer];
            }
        } else {
            printf("Invalid move! Please choose an empty position (1-9).\n");
        }
    }
    
    printf("Game Over!\n");
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        TicTacToeGame *game = [[TicTacToeGame alloc] init];
        [game playGame];
        [game release];
    }
    
    return 0;
}
