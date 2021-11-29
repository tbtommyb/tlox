#include "cloc.h"
#include <check.h>
#include <config.h>
#include <stdint.h>
#include <stdlib.h>

static char *readFile(const char *path) {
  FILE *file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Could not open file \"%s\".\n", path);
    exit(74);
  }

  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  rewind(file);

  char *buffer = (char *)malloc(fileSize + 1);
  if (buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
    exit(75);
  }

  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  if (bytesRead < fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path);
    exit(76);
  }
  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

START_TEST(simpleAddition) {
  initVM();

  const char *input = "print 5 + 5;\n";
  FILE *output = tmpfile();

  interpret(input, output);

  const int outputLength = ftell(output);
  char *outputBuffer = calloc(outputLength + 1, sizeof(char));
  rewind(output);
  fgets(outputBuffer, outputLength, output);
  fclose(output);

  const char *expected = "10\0";
  ck_assert_msg(strcmp(outputBuffer, expected) == 0,
                "Expected %s, but got %.*s", expected, outputLength,
                outputBuffer);
  freeVM();
}
END_TEST

START_TEST(simpleConcatenation) {
  initVM();

  const char *input = "print \"hello \" + \"world\";";
  FILE *output = tmpfile();

  interpret(input, output);

  const int outputLength = ftell(output);
  char *outputBuffer = calloc(outputLength + 1, sizeof(char));
  rewind(output);
  fgets(outputBuffer, outputLength, output);
  fclose(output);

  const char *expected = "hello world\0";
  ck_assert_msg(strcmp(outputBuffer, expected) == 0,
                "Expected %s, but got %.*s", expected, outputLength,
                outputBuffer);
  freeVM();
}
END_TEST

START_TEST(fileInheritance) {
  initVM();

  const char *input = readFile("./tests/testCases/simpleInheritance.lox");
  FILE *output = tmpfile();

  interpret(input, output);

  const int outputLength = ftell(output);
  char *outputBuffer = calloc(outputLength + 1, sizeof(char));
  rewind(output);
  fgets(outputBuffer, outputLength, output);
  fclose(output);
  const char *expected = "Dunk in the fryer. Finish with icing\0";

  ck_assert_msg(strcmp(outputBuffer, expected) == 0,
                "Expected '%s', but got '%.*s'", expected, outputLength,
                outputBuffer);
  freeVM();
}
END_TEST

Suite * singleLineInputSuite(void)
{
    Suite *s;
    TCase *arithmeticTestCases;
    TCase *stringsTestCases;

    s = suite_create("SingleLineInputs");

    // Arithmetic tests
    arithmeticTestCases = tcase_create("Arithmetic");

    tcase_add_test(arithmeticTestCases, simpleAddition);
    suite_add_tcase(s, arithmeticTestCases);

    // Strings tests
    stringsTestCases = tcase_create("Strings");

    tcase_add_test(stringsTestCases, simpleConcatenation);
    suite_add_tcase(s, stringsTestCases);

    return s;
}

Suite * fileInputSuite(void)
{
    Suite *s;
    TCase *classesTestCases;

    s = suite_create("FileInputs");

    // Classes tests
    classesTestCases = tcase_create("Classes");

    tcase_add_test(classesTestCases, fileInheritance);
    suite_add_tcase(s, classesTestCases);

    return s;
}

int main(void) {
  SRunner *suiteRunner = srunner_create(singleLineInputSuite());
  srunner_add_suite(suiteRunner, fileInputSuite());

  srunner_run_all(suiteRunner, CK_ENV);
  int testFailCount = srunner_ntests_failed(suiteRunner);
  srunner_free(suiteRunner);

  return testFailCount == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
