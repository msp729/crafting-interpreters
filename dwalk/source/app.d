import std.stdio;

import lox.lox;

int main(string[] args)
{
    writeln(args);
    if (args.length > 2)
    {
        writeln("Usage: jlox [script]");
        return 64;
    }
    else if (args.length == 2)
    {
        return Lox.runFile(args[1]);
    }
    else
    {
        return Lox.runPrompt();
    }
}
