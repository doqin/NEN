using NEN.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    internal class Parser(string moduleName, string[] contentLines, Token[] tokens)
    {
        private readonly string moduleName = moduleName;
        private readonly Token[] tokens = tokens;
        private int index = 0;
        private readonly string[] content = contentLines;
        public Module Parse()
        {
            List<Class> classes = [];
            while (index < tokens.Length)
            {
                var token = Consume();
                switch (token!.Type)
                {
                    case TokenType.Keyword:
                        switch (token.Value)
                        {
                            case "lớp":
                                classes.Add(ParseClass());
                                break;
                            default:
                                throw new ExpectedException(content, "lớp", token.Line, token.Column);
                        }
                        break;
                    default:
                        throw new ExpectedException(content, "lớp", token.Line, token.Column);
                }
            }
            return new Module { Name = moduleName, Classes = [ .. classes] };
        }

        private Class ParseClass()
        {
            var (line, column) = GetCurrentPosition();
            var classIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên lớp");
            List<Method> methods = [];
            while(Current() != null && Current()!.Value != "kết_thúc")
            {
                var token = Consume();
                switch(token!.Value)
                {
                    case "phương_thức":
                        methods.Add(ParseMethod());
                        break;
                    default:
                        throw new UnexpectedException(content, token.Value, token.Line, token.Column);
                }
            }
            if (Current() == null) OutOfTokenHelper("kết_thúc");
            Consume();
            return new Class { Name = classIdentifier!.Value, Methods = [.. methods], Line = line, Column = column };
        }

        private Method ParseMethod()
        {
            var (line, column) = GetCurrentPosition();
            var methodIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên phương thức");
            var lParen = ConsumeOrThrow(TokenType.Punctuator, "(");
            // TODO: Parse parameters
            List<Variable> parameters = [];
            var rParen = ConsumeOrThrow(TokenType.Punctuator, ")");
            var arrow = ConsumeOrThrow(TokenType.Operator, "->");
            var returnTypeIdentifier = ConsumeOrThrow(TokenType.Identifier, "kiểu trả về");
            List<Statement> statements = [];
            while(Current() != null && Current()!.Value != "kết_thúc")
            {
                ParseStatements();
            }
            if (Current() == null) OutOfTokenHelper("kết_thúc");
            Consume();
            return new Method { Name = methodIdentifier.Value, ReturnType = returnTypeIdentifier.Value, Statements = [.. statements], Line = line, Column = column };
        }

        private void ParseStatements()
        {
            throw new NotImplementedException();
        }

        private Token ConsumeOrThrow(TokenType expectedTokenType, string expected)
        {
            var token = Consume();
            if (token == null) OutOfTokenHelper(expected);
            else if (token.Type != expectedTokenType) throw new ExpectedException(content, expected, token.Line, token.Column);
            return token!;
        }

        private void OutOfTokenHelper(string expected)
        {
            throw new ExpectedException(
                content,
                expected,
                GetCurrentLine(),
                GetCurrentColumn() + GetCurrentLength() + 1
            );
        }

        private Token? Current()
        {
            if (index < tokens.Length)
            {
                return tokens[index];
            }
            else
            {
                return null;
            }
        }

        private Token? Peek() {
            if (index + 1 < tokens.Length)
            {
                return tokens[index + 1];
            }
            else
            {
                return null;
            }
        }
        private Token? Consume()
        {
            if (index < tokens.Length)
            {
                return tokens[index++];
            }
            else
            {
                return null;
            }
        }

        private int GetCurrentLine()
        {
            return tokens[index].Line; 
        }
        private int GetCurrentColumn()
        {
            return tokens[index].Column;
        }
        private int GetCurrentLength()
        {
            return tokens[index].Value.Length;
        }
        private (int, int) GetCurrentPosition()
        {
            return (tokens[index].Line, tokens[index].Column);
        }
    }
}
