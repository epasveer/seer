#include<stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main() {
    pid_t pid1 = fork();

    if (pid1 < 0) {
        // Error occurred while forking
        perror("Fork failed");
        return 1;
    } else if (pid1 == 0) {
        // First child process
        printf("First child process is running\n");

        sleep(200000);

        printf("First child process completed\n");
        return 0;
    } else {
        // Parent process
        printf("Parent process is running\n");

        pid_t pid2 = fork();

        if (pid2 < 0) {
            // Error occurred while forking
            perror("Fork failed");
            return 1;
        } else if (pid2 == 0) {
            // Second child process
            printf("Second child process is running\n");

            // Simulate a crash by accessing an invalid memory address
            int *ptr = NULL;
            *ptr = 10;

            printf("This line will not be executed\n");
            return 0;
        } else {
            // Parent process

            int status2;
            waitpid(pid2, &status2, 0);

            if (WIFEXITED(status2)) {
                printf("Second child process exited with status: %d\n", WEXITSTATUS(status2));
            } else if (WIFSIGNALED(status2)) {
                printf("Second child process terminated by signal: %d\n", WTERMSIG(status2));
            }

            // Wait for the first child process to finish
            int status1;
            waitpid(pid1, &status1, 0);

            if (WIFEXITED(status1)) {
                printf("First child process exited with status: %d\n", WEXITSTATUS(status1));
            } else if (WIFSIGNALED(status1)) {
                printf("First child process terminated by signal: %d\n", WTERMSIG(status1));
            }


            sleep(500000);
        }
    }

    return 0;
}
