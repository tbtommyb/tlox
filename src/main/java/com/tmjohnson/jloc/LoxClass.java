package com.tmjohnson.jloc;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

class LoxClass implements LoxCallable {
    final String name;
    private final Map<String, LoxFunction> methods;
    private final Map<String, LoxFunction> classMethods;

    LoxClass(String name, Map<String, LoxFunction> methods) {
        this.name = name;
        this.methods = methods;
        this.classMethods = new HashMap<String, LoxFunction>();
    }

    LoxClass(String name, Map<String, LoxFunction> methods, Map<String, LoxFunction> classMethods) {
        this.name = name;
        this.methods = methods;
        this.classMethods = classMethods;
    }

    Object get(Token name) {
        LoxFunction method = classMethods.get(name.lexeme);
        if (method != null) {
            return method.bind(this);
        }

        throw new RuntimeError(name, "Undefined class method '" + name.lexeme + "'.");
    }

    LoxFunction findMethod(String name) {
        if (methods.containsKey(name)) {
            return methods.get(name);
        }

        return null;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public Object call(Interpreter interpreter, List<Object> arguments) {
        LoxInstance instance = new LoxInstance(this);
        LoxFunction initializer = findMethod("init");
        if (initializer != null) {
            initializer.bind(instance).call(interpreter, arguments);
        }
        return instance;
    }

    @Override
    public int arity() {
        LoxFunction initializer = findMethod("init");
        if (initializer == null) {
            return 0;
        }
        return initializer.arity();
    }
}
