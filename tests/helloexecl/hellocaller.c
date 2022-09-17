#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {

  pid_t pid    = fork();
  int   status = 0;

  if (pid == 0) {

    execlp("./hellocallee", "echo", "aa", "bb", (char*)0);
    abort();

  }else{

    printf("parent %d waiting for %d\n", getpid(), pid);
    waitpid(pid, &status, 0);
    printf("child %d exited %d\n", pid, status);

  }

  return status;
}
