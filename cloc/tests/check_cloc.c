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

void runReplTest(const char *input, const char *expected) {
  FILE *output = tmpfile();
  FILE *err = tmpfile();
  initVM(output, err);

  interpret(input);

  const int outputLength = ftell(output);
  char *outputBuffer = calloc(outputLength + 1, sizeof(char));
  rewind(output);
  fgets(outputBuffer, outputLength, output);
  fclose(output);

  ck_assert_msg(strcmp(outputBuffer, expected) == 0,
                "Expected %s, but got %.*s", expected, outputLength,
                outputBuffer);

  freeVM();
}

START_TEST(simpleConcatenation) {
  runReplTest("print \"hello \" + \"world\";", "hello world\0");
}
END_TEST
START_TEST(simpleAddition) { runReplTest("print 5 + 5;\n", "10\0"); }
END_TEST

START_TEST(ternary) {
  runReplTest("print 3 < 2 ? \"broken\" : 4 == 5 ? 55 : 99;\n", "99\0");
}
END_TEST

START_TEST(moduloOdd) { runReplTest("print 5 % 2;\n", "1\0"); }
END_TEST

START_TEST(moduloEven) { runReplTest("print 4 % 2;\n", "0\0"); }
END_TEST

Suite *singleLineInputSuite(void) {
  Suite *s;
  TCase *testCases;

  s = suite_create("SingleLineInputs");
  testCases = tcase_create("Cases");

  tcase_add_test(testCases, simpleAddition);
  tcase_add_test(testCases, simpleConcatenation);
  tcase_add_test(testCases, ternary);
  tcase_add_test(testCases, moduloOdd);
  tcase_add_test(testCases, moduloEven);

  suite_add_tcase(s, testCases);

  return s;
}

void runFileTest(const char *path, const char *expected) {
  const char *input = readFile(path);
  FILE *output = tmpfile();
  FILE *err = tmpfile();

  initVM(output, err);
  interpret(input);

  fseek(output, 0, SEEK_END);
  const int outputLength = ftell(output);
  rewind(output);

  char *outputBuffer = calloc(outputLength + 1, sizeof(char));
  rewind(output);
  fgets(outputBuffer, outputLength, output);

  fclose(output);
  fclose(err);

  ck_assert_msg(strcmp(outputBuffer, expected) == 0,
                "Expected '%s', but got '%.*s'", expected, outputLength,
                outputBuffer);

  freeVM();
}

void runFileExpectedErrorTest(const char *path, const char *expected) {
  const char *input = readFile(path);
  FILE *output = tmpfile();
  FILE *err = tmpfile();

  initVM(output, err);
  interpret(input);

  const int errLength = ftell(err);
  rewind(err);

  char *errBuffer = calloc(errLength + 1, sizeof(char));
  fgets(errBuffer, errLength, err);

  fclose(err);
  fclose(output);

  ck_assert_msg(strcmp(errBuffer, expected) == 0,
                "Expected '%s', but got '%.*s'", expected, errLength,
                errBuffer);

  freeVM();
}

START_TEST(fileInheritance) {
  runFileTest("./tests/testCases/simpleInheritance.lox",
              "Dunk in the fryer. Finish with icing\0");
}
END_TEST

START_TEST(arrays) { runFileTest("./tests/testCases/arrays.lox", "27\0"); }
END_TEST

START_TEST(fibonacci) { runFileTest("./tests/testCases/fibonacci.lox", "8\0"); }
END_TEST

START_TEST(closures) {
  runFileTest("./tests/testCases/closures.lox", "3030\0");
}
END_TEST

START_TEST(computedProperties) {
  runFileTest("./tests/testCases/computedProperties.lox",
              "Louise Armstrong plays the super trumpet\0");
}
END_TEST

START_TEST(continueTest) {
  runFileTest("./tests/testCases/continue.lox", "165\0");
}
END_TEST

START_TEST(increment) {
  runFileTest("./tests/testCases/increment.lox", "105\0");
}
END_TEST

START_TEST(switchTest) {
  runFileTest("./tests/testCases/switch.lox", "One Two unknown number\0");
}
END_TEST

START_TEST(reassignConstTest) {
  runFileExpectedErrorTest(
      "./tests/testCases/errors/const.lox",
      "[line 2] Error at '=': Cannot reassign constant variable.\0");
}
END_TEST

START_TEST(continueOutsideLoopTest) {
  runFileExpectedErrorTest(
      "./tests/testCases/errors/continue.lox",
      "[line 2] Error at 'continue': Cannot use 'continue' outside a loop\0");
}
END_TEST

START_TEST(returnOutsideFunctionTest) {
  runFileExpectedErrorTest(
      "./tests/testCases/errors/return.lox",
      "[line 1] Error at 'return': Can't return from top-level code.\0");
}
END_TEST

START_TEST(returnFromInitialiserTest) {
  runFileExpectedErrorTest("./tests/testCases/errors/returnFromInitialiser.lox",
                           "[line 4] Error at 'return': Can't return a value "
                           "from an initializer.\0");
}
END_TEST

START_TEST(duplicateConstTest) {
  runFileExpectedErrorTest("./tests/testCases/errors/duplicateConst.lox",
                           "[line 2] Error at 'a': Already a variable with "
                           "this name in this scope\0");
}
END_TEST

START_TEST(constOverrideTest) {
  runFileExpectedErrorTest(
      "./tests/testCases/errors/constOverride.lox",
      "[line 2] Error at 'b': Cannot redeclare a const variable\0");
}
END_TEST

Suite *fileInputSuite(void) {
  Suite *s;
  TCase *testCases;
  TCase *errorTestCases;

  s = suite_create("FileInputs");
  testCases = tcase_create("Cases");

  tcase_add_test(testCases, fileInheritance);
  tcase_add_test(testCases, arrays);
  tcase_add_test(testCases, fibonacci);
  tcase_add_test(testCases, closures);
  tcase_add_test(testCases, computedProperties);
  tcase_add_test(testCases, continueTest);
  tcase_add_test(testCases, increment);
  tcase_add_test(testCases, switchTest);

  errorTestCases = tcase_create("Expected error cases");
  tcase_add_test(errorTestCases, reassignConstTest);
  tcase_add_test(errorTestCases, continueOutsideLoopTest);
  tcase_add_test(errorTestCases, returnOutsideFunctionTest);
  tcase_add_test(errorTestCases, returnFromInitialiserTest);
  tcase_add_test(errorTestCases, duplicateConstTest);
  tcase_add_test(errorTestCases, constOverrideTest);

  suite_add_tcase(s, testCases);
  suite_add_tcase(s, errorTestCases);

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
