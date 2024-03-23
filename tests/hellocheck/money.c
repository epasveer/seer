#include "money.h"
#include <stdlib.h>
#include <cstring>

struct Money {
    int amount;
    char currency[132];
};

Money* money_create (int amount, const char* currency) {

    Money *m = (Money*)malloc(sizeof(Money));

    if (m == NULL) {
        return NULL;
    }

    m->amount = amount;
    strcpy(m->currency, currency);

    return m;
}

int money_amount (Money* m) {
    return m->amount;
}

char* money_currency (Money* m) {
    return m->currency;
}

void money_free (Money* m) {
  free(m);
}

