#include <GL/freeglut.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

#define MAP_WIDTH 24
#define MAP_HEIGHT 24
#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480
#define FOV 60.0

int maze[MAP_WIDTH][MAP_HEIGHT];
int inputNumbers[6];
float playerX = 12.0f, playerY = 12.0f;
float playerAngle = 0.0f;
int gameOver = 0;

void generateMazeFromNumbers() {
    srand(inputNumbers[0] + inputNumbers[1]*2 + inputNumbers[2]*3 +
          inputNumbers[3]*4 + inputNumbers[4]*5 + inputNumbers[5]*6);

    for (int y = 0; y < MAP_HEIGHT; y++) {
        for (int x = 0; x < MAP_WIDTH; x++) {
            if (x == 0 || y == 0 || x == MAP_WIDTH-1 || y == MAP_HEIGHT-1)
                maze[x][y] = 1;
            else
                maze[x][y] = rand() % 5 == 0 ? 1 : 0;
        }
    }

    maze[1][1] = 0; // Start
    maze[MAP_WIDTH-2][MAP_HEIGHT-2] = 0; // Exit
}

float gaussianNoise() {
    static int hasSpare = 0;
    static double spare;
    if (hasSpare) {
        hasSpare = 0;
        return spare;
    }

    hasSpare = 1;
    double u, v, s;
    do {
        u = (rand() / ((double)RAND_MAX)) * 2.0 - 1.0;
        v = (rand() / ((double)RAND_MAX)) * 2.0 - 1.0;
        s = u * u + v * v;
    } while (s >= 1.0 || s == 0.0);

    s = sqrt(-2.0 * log(s) / s);
    spare = v * s;
    return u * s;
}

void drawNoiseWall(float x, float y, float height) {
    glBegin(GL_QUADS);
    for (float i = 0; i < height; i += 1.0f) {
        float brightness = fabs(gaussianNoise());
        glColor3f(brightness, brightness, brightness);
        glVertex2f(x, y + i);
        glVertex2f(x + 1, y + i);
        glVertex2f(x + 1, y + i + 1);
        glVertex2f(x, y + i + 1);
    }
    glEnd();
}

void display() {
    if (gameOver) return;

    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();

    for (int ray = 0; ray < SCREEN_WIDTH; ray++) {
        float rayAngle = (playerAngle - FOV/2.0f) + ((float)ray / SCREEN_WIDTH) * FOV;
        float rayX = cos(rayAngle * M_PI / 180.0);
        float rayY = sin(rayAngle * M_PI / 180.0);

        float distance = 0.0f;
        while (distance < 20.0f) {
            int testX = (int)(playerX + rayX * distance);
            int testY = (int)(playerY + rayY * distance);

            if (testX < 0 || testX >= MAP_WIDTH || testY < 0 || testY >= MAP_HEIGHT) break;
            if (maze[testX][testY] == 1) break;

            distance += 0.1f;
        }

        float wallHeight = SCREEN_HEIGHT / (distance + 0.1f);
        drawNoiseWall(ray, SCREEN_HEIGHT/2 - wallHeight/2, wallHeight);
    }

    glutSwapBuffers();
}

void timer(int value) {
    glutPostRedisplay();
    glutTimerFunc(16, timer, 0);
}

void keyboard(unsigned char key, int x, int y) {
    float moveSpeed = 0.2f;
    float rotSpeed = 5.0f;

    if (key == 'w') {
        float newX = playerX + cos(playerAngle * M_PI / 180.0) * moveSpeed;
        float newY = playerY + sin(playerAngle * M_PI / 180.0) * moveSpeed;
        if (maze[(int)newX][(int)newY] == 0) {
            playerX = newX;
            playerY = newY;
        }
    }
    if (key == 's') {
        float newX = playerX - cos(playerAngle * M_PI / 180.0) * moveSpeed;
        float newY = playerY - sin(playerAngle * M_PI / 180.0) * moveSpeed;
        if (maze[(int)newX][(int)newY] == 0) {
            playerX = newX;
            playerY = newY;
        }
    }
    if (key == 'a') playerAngle -= rotSpeed;
    if (key == 'd') playerAngle += rotSpeed;

    if (key == 'q' || ((int)playerX == MAP_WIDTH-2 && (int)playerY == MAP_HEIGHT-2)) {
        gameOver = 1;
        glutPostRedisplay();
        sleep(1); // Show final image for 1 sec.
        exit(0);
    }
}

void printAsciiMaze() {
    printf("\nASCII Maze Preview:\n");
    for (int y = 0; y < MAP_HEIGHT; y++) {
        for (int x = 0; x < MAP_WIDTH; x++) {
            if (x == 1 && y == 1)
                printf("S");
            else if (x == MAP_WIDTH - 2 && y == MAP_HEIGHT - 2)
                printf("E");
            else if (maze[x][y] == 1)
                printf("#");
            else
                printf(".");
        }
        printf("\n");
    }

    printf("'a'=Rotate left\n");
    printf("'d'=Rotate right\n");
    printf("'w'=Forward\n");
    printf("'s'=Backward\n");
    printf("'q'=Quit\n");

    printf("\nPress SPACE to start the game...\n");

    // Wait for SPACE
    while (1) {
        char c = getchar();
        if (c == ' ') break;
    }
}


int main(int argc, char** argv) {

    printf("Enter 6 numbers (1-59) to seed randomization:\n");
    for (int i = 0; i < 6; i++) {
        scanf("%d", &inputNumbers[i]);
        if (inputNumbers[i] < 1 || inputNumbers[i] > 59) {
            printf("Invalid input. Must be 1-59.\n");
            return 1;
        }
    }

    generateMazeFromNumbers();
    printAsciiMaze();

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(SCREEN_WIDTH, SCREEN_HEIGHT);
    glutCreateWindow("Noise Maze Raycaster");

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0, SCREEN_WIDTH, 0, SCREEN_HEIGHT);
    glMatrixMode(GL_MODELVIEW);

    glutDisplayFunc(display);
    glutKeyboardFunc(keyboard);
    glutTimerFunc(0, timer, 0);

    glutMainLoop();
    return 0;
}

