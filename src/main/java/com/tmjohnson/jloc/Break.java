package com.tmjohnson.jloc;

class Break extends RuntimeException {
    Break() {
        super("Breaking out of loop");
    }
}
