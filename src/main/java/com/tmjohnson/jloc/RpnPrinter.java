package com.tmjohnson.jloc;

import java.util.stream.Collectors;
import java.util.stream.Stream;

class RpnPrinter implements Expr.Visitor<String> {
    String print(Expr expr) {
        return expr.accept(this);
    }

    @Override
    public String visitBinaryExpr(Expr.Binary expr) {
        return toRpn(expr.operator.lexeme, expr.left, expr.right);
    }

    @Override
    public String visitGroupingExpr(Expr.Grouping expr) {
        return toRpn(null, expr.expression);
    }

    @Override
    public String visitLiteralExpr(Expr.Literal expr) {
        if (expr.value == null) {
            return "nil";
        }
        return expr.value.toString();
    }

    @Override
    public String visitUnaryExpr(Expr.Unary expr) {
        return toRpn(expr.operator.lexeme, expr.right);
    }

    @Override
    public String visitTernaryExpr(Expr.Ternary expr) {
        return toRpn("?:", expr.condition, expr.thenClause, expr.elseClause);
    }

    private String toRpn(String name, Expr... exprs) {
        StringBuilder builder = new StringBuilder();

        String els = Stream.of(exprs).map(expr -> expr.accept(this)).collect(Collectors.joining(" "));
        builder.append(els);

        if (name != null) {
            builder.append(" ").append(name);
        }
        return builder.toString();
    }

    public static void main(String[] args) {
        Expr expression = new Expr.Binary(
                new Expr.Grouping(new Expr.Binary(new Expr.Literal(1), new Token(TokenType.PLUS, "+", null, 1),
                        new Expr.Literal(2))),
                new Token(TokenType.STAR, "*", null, 1), new Expr.Grouping(new Expr.Binary(new Expr.Literal(4),
                        new Token(TokenType.MINUS, "-", null, 1), new Expr.Literal(3))));

        System.out.println(new RpnPrinter().print(expression));
    }
}
