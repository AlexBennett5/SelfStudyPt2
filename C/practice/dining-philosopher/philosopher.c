#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct philosopher {
  const char *name;
  pthread_mutex_t *leftfork;
  pthread_mutex_t *rightfork;
} philosopher;

void* dine(void *phil_ptr) {

  int tries;
  int status = 0;
  philosopher *phil = (philosopher *) phil_ptr;
  pthread_mutex_t *fork1, *fork2, *tmpfork;

  while(1) {
    printf("%s is thinking, not eating\n", phil->name);
    sleep(1 + (rand() % 5));

    fork1 = phil->leftfork;
    fork2 = phil->rightfork;
    tries = 3;

    while(1) {
      pthread_mutex_lock(fork1);
      printf("%s: fork1 grabbed\n", phil->name);
      status = (tries > 0) ?  pthread_mutex_trylock(fork2) : pthread_mutex_lock(fork2);
      printf("%s: fork2 attempted\n", phil->name);

      if (status) {
        printf("%s: try failed\n", phil->name);
        pthread_mutex_unlock(fork1);
        tries--;
      } else {
        printf("%s: try succeeded\n", phil->name);
        break;
      }

    }

    printf("%s is eating\n", phil->name);
    sleep(1 + (rand() % 5));
    printf("%s is done eating\n", phil->name);
    pthread_mutex_unlock(fork1);
    pthread_mutex_unlock(fork2);
  }

  return NULL;

}

void dinner_table() {
  const char *names[6] = { "Plato", "Hegel", "Zizek", "Confucius", "Kant", "Marx" };
  pthread_mutex_t forks[6];
  pthread_t tid[6];
  philosopher phils[6];
  philosopher *phil;

  for (int i = 0; i < 6; i++) {
    pthread_mutex_init(&forks[i], NULL);
  }

  for (int i = 0; i < 6; i++) {
    phil = &phils[i];
    phil->name = names[i];
    phil->leftfork = &forks[i % 6];
    phil->rightfork = &forks[(i + 1) % 6];

    if (pthread_create(&tid[i], NULL, dine, phil)) {
      fprintf(stderr, "Error creating thread\n");
      return;
    }
  }

  for (int i = 0; i < 6; i++) {
    pthread_join(tid[i], NULL);
  }

}

int main() {
  dinner_table();
  return 0;
}
