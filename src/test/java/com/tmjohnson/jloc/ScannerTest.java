package com.tmjohnson.jloc;

import static com.tmjohnson.jloc.TokenType.CLASS;
import static com.tmjohnson.jloc.TokenType.ELSE;
import static com.tmjohnson.jloc.TokenType.EOF;
import static com.tmjohnson.jloc.TokenType.IDENTIFIER;
import static com.tmjohnson.jloc.TokenType.LEFT_PAREN;
import static com.tmjohnson.jloc.TokenType.NUMBER;
import static com.tmjohnson.jloc.TokenType.PLUS;
import static com.tmjohnson.jloc.TokenType.RIGHT_PAREN;
import static org.junit.Assert.assertArrayEquals;

import java.util.List;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void canScanSimpleInput() {
        Token[] expected = { new Token(NUMBER, "1", 1.0, 1), new Token(PLUS, "+", null, 1),
                new Token(CLASS, "class", null, 1), new Token(ELSE, "else", null, 1),
                new Token(IDENTIFIER, "myname", null, 1), new Token(EOF, "", null, 1) };

        String source = "1 + class else myname // comment";
        Scanner scanner = new Scanner(source);
        List<Token> output = scanner.scanTokens();
        assertArrayEquals(output.toArray(), expected);
    }

    @Test
    public void canScanMultilineInput() {
        Token[] expected = { new Token(NUMBER, "1.23", 1.23, 1), new Token(LEFT_PAREN, "(", null, 4),
                new Token(CLASS, "class", null, 4), new Token(RIGHT_PAREN, ")", null, 4), new Token(EOF, "", null, 4) };

        String source = "1.23 /* multilinecomment \n lots of text \n on multiple lines \n*/ (class)";
        Scanner scanner = new Scanner(source);
        List<Token> output = scanner.scanTokens();
        assertArrayEquals(output.toArray(), expected);
    }
}
