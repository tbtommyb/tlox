package com.tmjohnson.jloc;

class BreakException extends RuntimeException {
    BreakException() {
        super("Breaking out of loop");
    }
}
