package com.tmjohnson.jloc;

import java.util.List;
import java.util.Map;

class LoxClass extends LoxInstance implements LoxCallable {
    final String name;
    final LoxClass superclass;
    private final Map<String, LoxFunction> methods;
    private final LoxModule module;

    LoxClass(LoxClass metaclass, String name, LoxClass superclass, Map<String, LoxFunction> methods) {
        super(metaclass);
        this.name = name;
        this.methods = methods;
        this.superclass = superclass;
        this.module = null;
    }

    LoxClass(LoxClass metaclass, String name, LoxClass superclass, Map<String, LoxFunction> methods, LoxModule module) {
        super(metaclass);
        this.name = name;
        this.methods = methods;
        this.superclass = superclass;
        this.module = module;
    }

    LoxFunction findMethod(String name) {
        if (methods.containsKey(name)) {
            return methods.get(name);
        }
        if (module != null) {
            LoxFunction moduleMethod = module.findMethod(name);
            if (moduleMethod != null) {
                return moduleMethod;
            }
        }
        if (superclass != null) {
            return superclass.findMethod(name);
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
