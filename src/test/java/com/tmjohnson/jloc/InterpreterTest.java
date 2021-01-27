package com.tmjohnson.jloc;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class InterpreterTest {
    private final PrintStream standardOut = System.out;
    private final ByteArrayOutputStream outputStreamCaptor = new ByteArrayOutputStream();
    private Lox lox;
    private Interpreter interpreter;
    private Scanner scanner;

    @Before
    public void setUp() {
        System.setOut(new PrintStream(outputStreamCaptor));

        lox = new Lox(ExecutionMode.REPL);
        interpreter = new Interpreter(lox);
        scanner = new Scanner(lox);
    }

    @After
    public void tearDown() {
        System.setOut(standardOut);
    }

    @Test
    public void canInterpretNestedTernaryExpression() {
        String input = "2 >= 1 ? 3 * 10 / 2 : -1;";
        String expected = "15";

        Parser parser = new Parser(lox, scanner.scanTokens(input));
        interpreter.interpret(parser.parse());

        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }

    @Test
    public void canInterpretAdditionAndNegativeMultiplication() {
        String input = "4 + 4 - (-1 * 3);";
        String expected = "11";

        Parser parser = new Parser(lox, scanner.scanTokens(input));
        interpreter.interpret(parser.parse());

        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }

    @Test
    public void canInterpretFancyLogic() {
        String input = "2 != 3 == !(5 < 10);";
        String expected = "false";

        Parser parser = new Parser(lox, scanner.scanTokens(input));
        interpreter.interpret(parser.parse());

        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }

    @Test
    public void canInterpretCommaOperator() {
        String input = "(2, 3, 4) == 4;";
        String expected = "true";

        Parser parser = new Parser(lox, scanner.scanTokens(input));
        interpreter.interpret(parser.parse());

        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }

    @Test
    public void canPerformGrossStringCoercion() {
        String input = "4 * 4 + \"i love javascript\";";
        String expected = "16i love javascript";

        Parser parser = new Parser(lox, scanner.scanTokens(input));
        interpreter.interpret(parser.parse());

        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }
}
