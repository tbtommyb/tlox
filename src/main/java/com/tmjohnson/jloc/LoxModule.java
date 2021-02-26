package com.tmjohnson.jloc;

import java.util.Map;

class LoxModule {
    final String name;
    public final Map<String, LoxFunction> methods;

    LoxModule(String name, Map<String, LoxFunction> methods) {
        this.name = name;
        this.methods = methods;
    }

    LoxFunction findMethod(String name) {
        if (methods.containsKey(name)) {
            return methods.get(name);
        }
        return null;
    }
}
