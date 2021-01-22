package com.tmjohnson.jloc;

import static com.tmjohnson.jloc.TokenType.CLASS;
import static com.tmjohnson.jloc.TokenType.ELSE;
import static com.tmjohnson.jloc.TokenType.EOF;
import static com.tmjohnson.jloc.TokenType.IDENTIFIER;
import static com.tmjohnson.jloc.TokenType.NUMBER;
import static com.tmjohnson.jloc.TokenType.PLUS;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void canScanSimpleInput() {
        List<Token> expected = new ArrayList<Token>(Arrays.asList(new Token(NUMBER, "1", 1.0, 1),
                new Token(PLUS, "+", null, 1), new Token(CLASS, "class", null, 1), new Token(ELSE, "else", null, 1),
                new Token(IDENTIFIER, "myname", null, 1), new Token(EOF, "", null, 1)));

        String source = "1 + class else myname // comment";
        Scanner scanner = new Scanner(source);
        List<Token> output = scanner.scanTokens();
        assertEquals(output, expected);
    }
}
