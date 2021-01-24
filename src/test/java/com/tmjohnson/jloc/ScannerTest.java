package com.tmjohnson.jloc;

import static org.junit.Assert.assertArrayEquals;

import java.util.List;

import org.junit.Test;

public class ScannerTest {
    @Test
    public void canScanSimpleInput() {
        Token[] expected = { new Token(TokenType.NUMBER, "1", 1.0, 1), new Token(TokenType.PLUS, "+", null, 1),
                new Token(TokenType.CLASS, "class", null, 1), new Token(TokenType.ELSE, "else", null, 1),
                new Token(TokenType.IDENTIFIER, "myname", null, 1), new Token(TokenType.EOF, "", null, 1) };

        String source = "1 + class else myname // comment";
        Scanner scanner = new Scanner(source);
        List<Token> output = scanner.scanTokens();
        assertArrayEquals(output.toArray(), expected);
    }

    @Test
    public void canScanMultilineInput() {
        Token[] expected = { new Token(TokenType.NUMBER, "1.23", 1.23, 1),
                new Token(TokenType.LEFT_PAREN, "(", null, 4), new Token(TokenType.CLASS, "class", null, 4),
                new Token(TokenType.RIGHT_PAREN, ")", null, 4), new Token(TokenType.EOF, "", null, 4) };

        String source = "1.23 /* multilinecomment \n lots of text \n on multiple lines \n*/ (class)";
        Scanner scanner = new Scanner(source);
        List<Token> output = scanner.scanTokens();
        assertArrayEquals(output.toArray(), expected);
    }
}
