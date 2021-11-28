#include "cloc.h"
#include <check.h>
#include <config.h>
#include <stdint.h>
#include <stdlib.h>

START_TEST(sanity_check) {
  initVM();

  const char *input = "print 5 + 5;\n";
  FILE *tmp = tmpfile();
  char buffer[100];

  interpret(input, tmp);

  const int tmp_length = ftell(tmp);
  printf("tmp_length %d\n", tmp_length);
  rewind(tmp);

  fgets(buffer, tmp_length, tmp);
  fclose(tmp);
  // printf("%.*s\n", tmp_length, buffer);

  ck_assert_msg(strcmp(buffer, "10\0") == 0, "expected output '10'");
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
