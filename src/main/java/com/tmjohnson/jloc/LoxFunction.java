package com.tmjohnson.jloc;

import java.util.List;

class LoxFunction implements LoxCallable {
    private final Token name;
    private final Expr.Fun declaration;
    private final Environment closure;

    LoxFunction(Token name, Expr.Fun declaration, Environment closure) {
        this.name = name;
        this.declaration = declaration;
        this.closure = closure;
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        Environment environment = new Environment(closure);

        for (int i = 0; i < declaration.params.size(); i++) {
            environment.define(declaration.params.get(i).lexeme, arguments.get(i));
        }
        try {
            interpreter.executeBlock(declaration.body, environment);
        } catch (Return returnValue) {
            return returnValue.value;
        }
        return null;
    }

    @Override
    public int arity() {
        return declaration.params.size();
    }

    @Override
    public String toString() {
        if (name == null) {
            return "<fn>";
        }
        return "<fn " + name.lexeme + ">";
    }
}
