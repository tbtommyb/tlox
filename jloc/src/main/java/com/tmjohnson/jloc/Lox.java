package com.tmjohnson.jloc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class Lox {
    private Interpreter interpreter;
    boolean hadError = false;
    boolean hadRuntimeError = false;
    ExecutionMode executionMode;

    public Lox(ExecutionMode executionMode) {
        this.executionMode = executionMode;
        interpreter = new Interpreter(this);
    }

    public static void main(String[] args) throws IOException {
        if (args.length > 1) {
            System.out.println("Usage: jlox [script]");
            System.exit(64);
        } else if (args.length == 1) {
            Lox lox = new Lox(ExecutionMode.BATCH);
            lox.runFile(args[0]);
        } else {
            Lox lox = new Lox(ExecutionMode.REPL);
            lox.runPrompt();
        }
    }

    private void runFile(String path) throws IOException {
        byte[] bytes = Files.readAllBytes(Paths.get(path));
        run(new String(bytes, Charset.defaultCharset()));
        if (hadError) {
            System.exit(65);
        }
        if (hadRuntimeError) {
            System.exit(70);
        }
    }

    private void runPrompt() throws IOException {
        InputStreamReader input = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(input);

        for (;;) {
            System.out.print("> ");
            String line = reader.readLine();
            if (line == null) {
                break;
            }
            run(line);
            hadError = false;
        }
    }

    public void run(String source) {
        Scanner scanner = new Scanner(this);
        List<Token> tokens = scanner.scanTokens(source);

        Parser parser = new Parser(this, tokens);
        List<Stmt> statements = parser.parse();

        if (hadError) {
            return;
        }

        Resolver resolver = new Resolver(this, interpreter);
        resolver.resolve(statements);

        if (hadError) {
            return;
        }

        interpreter.interpret(statements);
    }

    private void report(int line, String where, String message) {
        System.err.println("[line " + line + "] Error" + where + ": " + message);
        hadError = true;
    }

    void error(int line, String message) {
        report(line, "", message);
    }

    void error(Token token, String message) {
        if (token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "'", message);
        }
    }

    void runtimeError(RuntimeError error) {
        System.err.println(error.getMessage() + "\n[line " + error.token.line + "]");
        hadRuntimeError = true;
    }
}
