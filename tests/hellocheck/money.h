#ifndef MONEY_H
#define MONEY_H

typedef struct Money Money;

Money*  create_money        (int amount, const char* currenty);
int     money_amount        (Money* m);
char*   money_currency      (Money* m);
void    money_free          (Money* m);

#endif

