package com.tmjohnson.jloc;

import static com.tmjohnson.jloc.TokenType.BANG;
import static com.tmjohnson.jloc.TokenType.BANG_EQUAL;
import static com.tmjohnson.jloc.TokenType.EOF;
import static com.tmjohnson.jloc.TokenType.EQUAL_EQUAL;
import static com.tmjohnson.jloc.TokenType.FALSE;
import static com.tmjohnson.jloc.TokenType.GREATER;
import static com.tmjohnson.jloc.TokenType.GREATER_EQUAL;
import static com.tmjohnson.jloc.TokenType.LEFT_PAREN;
import static com.tmjohnson.jloc.TokenType.LESS;
import static com.tmjohnson.jloc.TokenType.LESS_EQUAL;
import static com.tmjohnson.jloc.TokenType.MINUS;
import static com.tmjohnson.jloc.TokenType.NIL;
import static com.tmjohnson.jloc.TokenType.NUMBER;
import static com.tmjohnson.jloc.TokenType.PLUS;
import static com.tmjohnson.jloc.TokenType.RIGHT_PAREN;
import static com.tmjohnson.jloc.TokenType.SLASH;
import static com.tmjohnson.jloc.TokenType.STAR;
import static com.tmjohnson.jloc.TokenType.STRING;
import static com.tmjohnson.jloc.TokenType.TRUE;

import java.util.List;

class Parser {
    private static class ParseError extends RuntimeException {
    }

    private final List<Token> tokens;
    private int current = 0;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    Expr parse() {
        try {
            return expression();
        } catch (ParseError error) {
            return null;
        }
    }

    private Expr expression() {
        return equality();
    }

    private Expr equality() {
        Expr expr = comparison();

        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr comparison() {
        Expr expr = term();

        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr term() {
        Expr expr = factor();

        while (match(MINUS, PLUS)) {
            Token operator = previous();
            Expr right = factor();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr factor() {
        Expr expr = unary();

        while (match(SLASH, STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr unary() {
        if (match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();
            return new Expr.Unary(operator, right);
        }

        return primary();
    }

    private Expr primary() {
        if (match(FALSE)) {
            return new Expr.Literal(false);
        }
        if (match(TRUE)) {
            return new Expr.Literal(true);
        }
        if (match(NIL)) {
            return new Expr.Literal(null);
        }
        if (match(NUMBER, STRING)) {
            return new Expr.Literal(previous().literal);
        }
        if (match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expected ')' after expression.");
            return new Expr.Grouping(expr);
        }
        throw error(peek(), "Expected expression.");
    }

    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private Token consume(TokenType type, String errorMessage) {
        if (check(type)) {
            return advance();
        }

        throw error(peek(), errorMessage);
    }

    private boolean check(TokenType type) {
        if (isAtEnd()) {
            return false;
        }
        return peek().type == type;
    }

    private Token advance() {
        if (!isAtEnd()) {
            current++;
        }
        return previous();
    }

    private boolean isAtEnd() {
        return peek().type == EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }

    private ParseError error(Token token, String message) {
        Lox.error(token, message);
        return new ParseError();
    }

    // Not yet actually used
    // private void synchronize() {
    // advance();

    // while (!isAtEnd()) {
    // if (previous().type == SEMICOLON) {
    // return;
    // }

    // switch (peek().type) {
    // case CLASS:
    // case FUN:
    // case VAR:
    // case FOR:
    // case IF:
    // case WHILE:
    // case PRINT:
    // case RETURN:
    // return;
    // }

    // advance();
    // }
    // }
}
