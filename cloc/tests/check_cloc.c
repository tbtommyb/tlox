#include "cloc.h"
#include <check.h>
#include <config.h>
#include <stdint.h>
#include <stdlib.h>

START_TEST(sanity_check) {
  initVM();

  const char *input = "print 5 + 5;\n";
  FILE *temp = tmpfile();

  interpret(input, temp);

  const int temp_length = ftell(temp);
  char buffer[100];
  rewind(temp);
  fgets(buffer, temp_length, temp);
  fclose(temp);

  char actual[temp_length + 1];
  sprintf(actual, "expected output: 10, got: %.*s\n", temp_length, buffer);

  ck_assert_msg(strcmp(buffer, "10\0") == 0, actual);
  freeVM();
}
END_TEST

int main(void) {
  Suite *s1 = suite_create("Core");
  TCase *tc1_1 = tcase_create("Core");
  SRunner *sr = srunner_create(s1);
  int nf;

  suite_add_tcase(s1, tc1_1);
  tcase_add_test(tc1_1, sanity_check);

  srunner_run_all(sr, CK_ENV);
  nf = srunner_ntests_failed(sr);
  srunner_free(sr);

  return nf == 0 ? 0 : 1;
}
