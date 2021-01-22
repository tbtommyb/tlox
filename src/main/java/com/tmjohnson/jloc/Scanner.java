package com.tmjohnson.jloc;

import static com.tmjohnson.jloc.TokenType.AND;
import static com.tmjohnson.jloc.TokenType.BANG;
import static com.tmjohnson.jloc.TokenType.BANG_EQUAL;
import static com.tmjohnson.jloc.TokenType.CLASS;
import static com.tmjohnson.jloc.TokenType.COMMA;
import static com.tmjohnson.jloc.TokenType.DOT;
import static com.tmjohnson.jloc.TokenType.ELSE;
import static com.tmjohnson.jloc.TokenType.EOF;
import static com.tmjohnson.jloc.TokenType.EQUAL;
import static com.tmjohnson.jloc.TokenType.EQUAL_EQUAL;
import static com.tmjohnson.jloc.TokenType.FALSE;
import static com.tmjohnson.jloc.TokenType.FOR;
import static com.tmjohnson.jloc.TokenType.FUN;
import static com.tmjohnson.jloc.TokenType.GREATER;
import static com.tmjohnson.jloc.TokenType.GREATER_EQUAL;
import static com.tmjohnson.jloc.TokenType.IDENTIFIER;
import static com.tmjohnson.jloc.TokenType.IF;
import static com.tmjohnson.jloc.TokenType.LEFT_BRACE;
import static com.tmjohnson.jloc.TokenType.LEFT_PAREN;
import static com.tmjohnson.jloc.TokenType.LESS;
import static com.tmjohnson.jloc.TokenType.LESS_EQUAL;
import static com.tmjohnson.jloc.TokenType.MINUS;
import static com.tmjohnson.jloc.TokenType.NIL;
import static com.tmjohnson.jloc.TokenType.NUMBER;
import static com.tmjohnson.jloc.TokenType.OR;
import static com.tmjohnson.jloc.TokenType.PLUS;
import static com.tmjohnson.jloc.TokenType.PRINT;
import static com.tmjohnson.jloc.TokenType.RETURN;
import static com.tmjohnson.jloc.TokenType.RIGHT_BRACE;
import static com.tmjohnson.jloc.TokenType.RIGHT_PAREN;
import static com.tmjohnson.jloc.TokenType.SEMICOLON;
import static com.tmjohnson.jloc.TokenType.SLASH;
import static com.tmjohnson.jloc.TokenType.STAR;
import static com.tmjohnson.jloc.TokenType.STRING;
import static com.tmjohnson.jloc.TokenType.SUPER;
import static com.tmjohnson.jloc.TokenType.THIS;
import static com.tmjohnson.jloc.TokenType.TRUE;
import static com.tmjohnson.jloc.TokenType.VAR;
import static com.tmjohnson.jloc.TokenType.WHILE;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Scanner {
    private static final Map<String, TokenType> keywords;

    static {
        keywords = new HashMap<>();
        keywords.put("and", AND);
        keywords.put("class", CLASS);
        keywords.put("else", ELSE);
        keywords.put("false", FALSE);
        keywords.put("for", FOR);
        keywords.put("fun", FUN);
        keywords.put("if", IF);
        keywords.put("nil", NIL);
        keywords.put("or", OR);
        keywords.put("print", PRINT);
        keywords.put("return", RETURN);
        keywords.put("super", SUPER);
        keywords.put("this", THIS);
        keywords.put("true", TRUE);
        keywords.put("var", VAR);
        keywords.put("while", WHILE);
    }

    private final String source;
    private final List<Token> tokens = new ArrayList<>();

    private int start = 0;
    private int current = 0;
    private int line = 1;

    Scanner(String source) {
        this.source = source;
    }

    List<Token> scanTokens() {
        while (!isAtEnd()) {
            start = current;
            scanToken();
        }

        tokens.add(new Token(EOF, "", null, line));
        return tokens;
    }

    private void scanToken() {
        char c = advance();
        switch (c) {
            case '(':
                addToken(LEFT_PAREN);
                break;
            case ')':
                addToken(RIGHT_PAREN);
                break;
            case '{':
                addToken(LEFT_BRACE);
                break;
            case '}':
                addToken(RIGHT_BRACE);
                break;
            case ',':
                addToken(COMMA);
                break;
            case '.':
                addToken(DOT);
                break;
            case '-':
                addToken(MINUS);
                break;
            case '+':
                addToken(PLUS);
                break;
            case ';':
                addToken(SEMICOLON);
                break;
            case '*':
                addToken(STAR);
                break;
            case '!':
                addToken(match('=') ? BANG_EQUAL : BANG);
                break;
            case '=':
                addToken(match('=') ? EQUAL_EQUAL : EQUAL);
                break;
            case '<':
                addToken(match('=') ? LESS_EQUAL : LESS);
                break;
            case '>':
                addToken(match('=') ? GREATER_EQUAL : GREATER);
                break;
            case '/':
                if (match('/')) {
                    while (peek() != '\n' && !isAtEnd())
                        advance();
                } else if (match('*')) {
                    multilineComment();
                } else {
                    addToken(SLASH);
                }
                break;
            case ' ':
            case '\r':
            case '\t':
                break;

            case '\n':
                line++;
                break;
            case '"':
                string();
                break;
            default:
                if (isDigit(c)) {
                    number();
                } else if (isAlpha(c)) {
                    identifier();
                } else {
                    Lox.error(line, "Unexpected character.");
                }
                break;
        }
    }

    private void identifier() {
        while (isAlphaNumeric(peek())) {
            advance();
        }

        String text = source.substring(start, current);
        TokenType type = keywords.get(text);
        if (type == null) {
            type = IDENTIFIER;
        }
        addToken(type);
    }

    private void number() {
        while (isDigit(peek())) {
            advance();
        }

        if (peek() == '.' && isDigit(peekNext())) {
            // Consume the "."
            advance();

            while (isDigit(peek())) {
                advance();
            }
        }

        addToken(NUMBER, Double.parseDouble(source.substring(start, current)));
    }

    private void string() {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') {
                line++;
            }
            advance();
        }

        if (isAtEnd()) {
            Lox.error(line, "Unterminated string.");
            return;
        }

        // The closing "
        advance();

        // Trim the surrounding quotes
        String value = source.substring(start + 1, current - 1);
        addToken(STRING, value);
    }

    private boolean match(char expected) {
        if (isAtEnd()) {
            return false;
        }
        if (source.charAt(current) != expected) {
            return false;
        }

        current++;
        return true;
    }

    private void multilineComment() {
        while (!(peek() == '*' && peekNext() == '/') && !isAtEnd()) {
            if (peek() == '\n') {
                line++;
            }
            advance();
        }

        if (isAtEnd()) {
            Lox.error(line, "Unterminated multiline comment.");
            return;
        }

        advance(); // The closing *
        advance(); // The closing /
    }

    private char peek() {
        if (isAtEnd()) {
            return '\0';
        }
        return source.charAt(current);
    }

    private char peekNext() {
        if (current + 1 >= source.length()) {
            return '\0';
        }
        return source.charAt(current + 1);
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAtEnd() {
        return current >= source.length();
    }

    private char advance() {
        current++;
        return source.charAt(current - 1);
    }

    private void addToken(TokenType type) {
        addToken(type, null);
    }

    private void addToken(TokenType type, Object literal) {
        String text = source.substring(start, current);
        tokens.add(new Token(type, text, literal, line));
    }
}
