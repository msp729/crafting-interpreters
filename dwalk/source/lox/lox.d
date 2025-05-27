module lox.lox;

import std.stdio;
import lox.scanner : Scanner;
import lox.token : Token;

class Lox
{
    static bool hadError = false;

    static int runFile(string path)
    {
        File f = File(path, "r");
        string contents;
        foreach (string line; f.lines())
        {
            contents ~= line;
        }
        run(contents);
        return hadError ? 65 : 0;
    }

    static int runPrompt()
    {
        write("> ");
        foreach (string line; stdin.lines)
        {
            write("> ");
            if (line == null)
                break;
            run(line);
            hadError = false;
        }

        return 0;
    }

    static int run(string source)
    {
        Scanner sc = Scanner(source);
        Token[] tokens = sc.scanTokens();

        foreach (Token tok; tokens)
            writeln(tok);
        return 0;
    }

    static void error(int line, string message)
    {
        report(line, "", message);
    }

    private static void report(int line, string where, string message)
    {
        writeln("[line ", line, "] Error", where, ": ", message);
        hadError = true;
    }
}
