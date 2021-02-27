package com.tmjohnson.jloc;

import java.util.Map;

class LoxModule {
    final Token name;
    public final Map<String, LoxFunction> methods;

    LoxModule(Token name, Map<String, LoxFunction> methods) {
        this.name = name;
        this.methods = methods;
    }

    LoxFunction findMethod(String name) {
        if (methods.containsKey(name)) {
            return methods.get(name);
        }
        return null;
    }

    @Override
    public String toString() {
        return name.lexeme;
    }
}
