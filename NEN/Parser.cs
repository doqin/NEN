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
            return new Module { Name = moduleName, Classes = [.. classes] };
        }

        private Class ParseClass()
        {
            var (line, column) = GetCurrentPosition();
            var classIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên lớp");
            List<Method> methods = [];
            while (Current() != null && Current()!.Value != "kết_thúc")
            {
                var token = Consume();
                switch (token!.Value)
                {
                    case "phương_thức":
                        methods.Add(ParseMethod());
                        break;
                    default:
                        UnexpectedHelper(token);
                        break;
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
            ConsumeOrThrow(TokenType.Punctuator, "(");
            // TODO: Parse parameters
            List<Variable> parameters = [];
            ConsumeOrThrow(TokenType.Punctuator, ")");
            ConsumeOrThrow(TokenType.Operator, "->");
            var returnTypeIdentifier = ConsumeOrThrow(TokenType.Identifier, "kiểu trả về");
            List<Statement> statements = [];
            while (Current() != null && Current()!.Value != "kết_thúc")
            {
                statements.Add(ParseStatement());
                ConsumeOrThrow(TokenType.Punctuator, ";");
            }
            if (Current() == null) OutOfTokenHelper("kết_thúc");
            Consume();
            return new Method { Name = methodIdentifier.Value, ReturnType = returnTypeIdentifier.Value, Statements = [.. statements], Line = line, Column = column };
        }

        private Statement ParseStatement()
        {
            var (line, column) = GetCurrentPosition();
            var token = Consume(); // this token is guaranteed not null from ParseMethod()
            switch (token!.Value)
            {
                case "biến":
                    return ParseVariableDeclarationStatement(line, column);
                default:
                    UnexpectedHelper(token);
                    throw new();
            }
        }

        private Statement ParseVariableDeclarationStatement(int line, int column)
        {
            var variableIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên biến");
            ConsumeOrThrow(TokenType.Keyword, "thuộc");
            var typeIdentifier = ConsumeOrThrow(TokenType.Identifier, "kiểu dữ liệu");
            Expression? initialValue = null;
            if (Current()?.Value == "gán")
            {
                ConsumeOrThrow(TokenType.Keyword, "gán"); // will never happen but ok
                initialValue = ParseExpression(0);
            }
            return new VariableDeclarationStatement
            {
                Variable = new Variable
                {
                    Name = variableIdentifier.Value,
                    Type = typeIdentifier.Value,
                    Line = variableIdentifier.Line,
                    Column = variableIdentifier.Column
                },
                InitialValue = initialValue,
                Line = line,
                Column = column
            };
        }

        private Expression ParseExpression(int minPrecedence)
        {
            var (line, column) = GetCurrentPosition();
            Expression left = ParsePrimary();
            while(true)
            {
                var precedence = CurrentPrecedence();
                if (precedence < minPrecedence)
                {
                    break;
                }
                var op = Consume()!; // should never be null
                var right = ParseExpression(precedence + 1);
                left = new BinaryExpression { Left = left, Operator = op.Value, Right = right, Line = line, Column = column };
            }
            return left;
        }

        private Expression ParsePrimary()
        {
            var (line, column) = GetCurrentPosition();
            var token = ConsumeOrThrow(TokenType.Literal | TokenType.Identifier, "biểu thức");
            return token.Type switch
            {
                TokenType.Literal => new LiteralExpression { Value = token.Value, Line = token.Line, Column = token.Column },
                // TODO: Handle variables and function call expressions
                TokenType.Identifier => throw new NotImplementedException(),
                _ => throw new(),// Should never happen
            };
        }

        /* Helpers */

        private void UnexpectedHelper(Token token)
        {
            throw new UnexpectedException(content, token.Value, token.Line, token.Column);
        }

        private Token ConsumeOrThrow(TokenType expectedTokenTypes, string expected)
        {
            var token = Consume();
            if (token == null) OutOfTokenHelper(expected);
            else if (!expectedTokenTypes.HasFlag(token.Type)) throw new ExpectedException(content, expected, token.Line, token.Column);
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

        private static readonly Dictionary<string, int> precedences = new()
        {
            { Operator.Plus, 1 },
            { Operator.Minus, 1},
            { Operator.Multiply, 2 },
            { Operator.Divide, 2 },
        };

        private int CurrentPrecedence()
        {
            Token? token = Current();
            if (token?.Type == TokenType.Operator || token?.Type == TokenType.Keyword)
            {
                if (precedences.TryGetValue(token.Value, out int value))
                {
                    return value;
                }
            }
            return -1;
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
