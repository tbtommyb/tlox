package com.tmjohnson.jloc;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class AstPrinterTest {
    private final PrintStream standardOut = System.out;
    private final ByteArrayOutputStream outputStreamCaptor = new ByteArrayOutputStream();

    @Before
    public void setUp() {
        System.setOut(new PrintStream(outputStreamCaptor));
    }

    @After
    public void tearDown() {
        System.setOut(standardOut);
    }

    @Test
    public void canParseNestedTernaries() {
        String input = "1 < 3 ? -1 : false ? -2 : 3 == 3 ? true : -1";
        String expected = "(?: (< 1.0 3.0) (- 1.0) (?: false (- 2.0) (?: (== 3.0 3.0) true (- 1.0))))";

        AstPrinter.run(input);
        String output = outputStreamCaptor.toString();
        assertEquals(expected, output.substring(0, output.length() - 1));
    }
}
