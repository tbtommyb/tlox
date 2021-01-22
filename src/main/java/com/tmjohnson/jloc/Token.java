package com.tmjohnson.jloc;

import java.util.Objects;

class Token {
    final TokenType type;
    final String lexeme;
    final Object literal;
    final int line;

    Token(TokenType type, String lexeme, Object literal, int line) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
    }

    public String toString() {
        return type + " " + lexeme + " " + literal + " " + line;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null) {
            return false;
        }
        if (getClass() != o.getClass()) {
            return false;
        }
        Token other = (Token) o;

        return Objects.equals(type, other.type) && Objects.equals(lexeme, other.lexeme)
                && ((literal == null && other.literal == null) || literal.equals(other.literal)) && line == other.line;
    }
}
