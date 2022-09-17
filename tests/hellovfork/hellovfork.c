#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

int main () {

    pid_t pid = vfork();

    if (pid == 0) {
        printf("Hello from child! (%d)\n", getpid());
        sleep(2);
        exit(EXIT_SUCCESS);
    }

    // To Here and see the difference
    printf("Hello from parent! (%d)\n", getpid());

    // Comment from here to...
    // Parent waits process pid (child)
    int status;

    waitpid(pid, &status, 0);

    // Option is 0 since I check it later
    if (WIFSIGNALED(status)){
        printf("Error\n");

    }else if (WEXITSTATUS(status)){
        printf("Child exited normally.\n");
    }

    return 0;
}

