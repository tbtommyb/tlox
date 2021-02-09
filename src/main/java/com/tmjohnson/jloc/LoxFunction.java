package com.tmjohnson.jloc;

import java.util.List;

class LoxFunction implements LoxCallable {
    private final Stmt.Function declaration;
    private final Environment closure;
    private final boolean isInitializer;

    LoxFunction(Stmt.Function declaration, Environment closure, boolean isInitializer) {
        this.isInitializer = isInitializer;
        this.declaration = declaration;
        this.closure = closure;
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        Environment environment = new Environment(closure);

        if (declaration.function.params != null) {
            for (int i = 0; i < declaration.function.params.size(); i++) {
                environment.define(declaration.function.params.get(i).lexeme, arguments.get(i));
            }
        }

        try {
            interpreter.executeBlock(declaration.function.body, environment);
        } catch (Return returnValue) {
            if (isInitializer) {
                return closure.getAt(0, "this");
            }
            return returnValue.value;
        }

        if (isInitializer) {
            return closure.getAt(0, "this");
        }
        return null;
    }

    LoxFunction bind(LoxInstance instance) {
        Environment environment = new Environment(closure);
        environment.define("this", instance);
        return new LoxFunction(declaration, environment, isInitializer);
    }

    LoxFunction bind(LoxClass klass) {
        Environment environment = new Environment(closure);
        return new LoxFunction(declaration, environment, false);
    }

    @Override
    public int arity() {
        return declaration.function.params.size();
    }

    @Override
    public String toString() {
        if (declaration.name == null) {
            return "<fn>";
        }
        return "<fn " + declaration.name + ">";
    }

    public boolean isGetter() {
        return declaration.function.params == null;
    }
}
