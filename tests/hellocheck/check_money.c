#include <check.h>
#include "money.h"
#include <stdlib.h>

extern Money* money_create(int, const char*);

START_TEST(test_money_create) {

    Money* m;

    m = money_create(5, "USD");

    ck_assert_int_eq(money_amount(m), 5);
    ck_assert_str_eq(money_currency(m), "USD");

    money_free(m);

} END_TEST


Suite* money_suite (void) {

    Suite *s;
    TCase *tc_core;

    s = suite_create("Money");

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_money_create);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void) {

    int no_failed = 0;

    Suite* s = money_suite();
    SRunner* runner = srunner_create(s);

    srunner_run_all(runner, CK_NORMAL);

    no_failed = srunner_ntests_failed(runner);
    srunner_free(runner);

    return (no_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}

