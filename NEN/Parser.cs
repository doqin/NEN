using NEN.Types;
using NEN.Exceptions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    public class Parser(string moduleName, string[] contentLines, Token[] tokens)
    {
        private readonly string moduleName = moduleName;
        private readonly Token[] tokens = tokens;
        private int index = 0;
        private readonly string[] content = contentLines;
        public Types.Module Parse()
        {
            List<ClassNode> classes = [];
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
            return new Types.Module { Name = moduleName, Classes = [.. classes] };
        }

        private ClassNode ParseClass()
        {
            var (line, column) = GetCurrentPosition();
            var classIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên lớp");
            List<MethodNode> methods = [];
            while (Current() != null && Current()!.Value != "kết_thúc")
            {
                var token = Consume();
                switch (token!.Value)
                {
                    case "@":
                        methods.Add(ParseMarker());
                        break;
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
            return new ClassNode { Name = classIdentifier!.Value, Methods = [.. methods], Line = line, Column = column };
        }

        private static string[] markers = [
            "Chính", "Tĩnh"    
        ];

        private MethodNode ParseMarker(MethodAttributes methodAttributes = MethodAttributes.Public, bool isEntryPoint = false)
        {
            var (line, column) = GetCurrentPosition();
            var annotationIdentifier = ConsumeOrThrow(TokenType.Identifier, "thuộc tính phương thức");
            if (!markers.Contains(annotationIdentifier.Value)) throw new ExpectedException(content, "thuộc tính phương thức", line, column);
            switch(annotationIdentifier.Value)
            {
                case "Chính":
                    if (isEntryPoint == true) throw new RedefinedException(content, "@Chính", line, column);
                    isEntryPoint = true;
                    break;
                case "Tĩnh":
                    if (methodAttributes.HasFlag(MethodAttributes.Static)) throw new RedefinedException(content, "@Tĩnh", line, column);
                    methodAttributes |= MethodAttributes.Static; 
                    break;
                default:
                    throw new NotImplementedException();
            }
            var token = ConsumeOrThrow(TokenType.Keyword | TokenType.Marker, "phương_thức");
            switch(token.Value)
            {
                case "@":
                    return ParseMarker(methodAttributes, isEntryPoint);
                case "phương_thức":
                    return ParseMethod(methodAttributes, isEntryPoint);
                default:
                    throw new ExpectedException(content, "phương_thức", GetCurrentLine(), GetCurrentColumn());
            }
        }

        private MethodNode ParseMethod(MethodAttributes methodAttributes = MethodAttributes.Public, bool isEntryPoint = false)
        {
            var (line, column) = GetCurrentPosition();
            var methodIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên phương thức");
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "(");
            // TODO: Parse parameters
            List<VariableNode> parameters = [];
            while (Current() != null && Current()!.Value != ")")
            {
                var parameterIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên tham số");
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thuộc");
                var type = ParseType();
                parameters.Add(new VariableNode { Name = parameterIdentifier.Value, Type = type, Line = parameterIdentifier.Line, Column = parameterIdentifier.Column });
                if (Current() != null && Current()!.Value != ")") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
            }
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
            ConsumeOrThrowIfNotEqual(TokenType.Operator, "->");
            var returnTypeIdentifier = ParseType();
            List<StatementNode> statements = [];
            while (Current() != null && Current()!.Value != "kết_thúc")
            {
                statements.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            if (Current() == null) OutOfTokenHelper("kết_thúc");
            Consume();
            return new MethodNode { IsEntryPoint = isEntryPoint, Attributes = methodAttributes, Name = methodIdentifier.Value, Parameters = [.. parameters], ReturnType = returnTypeIdentifier, Statements = [.. statements], Line = line, Column = column };
        }

        private StatementNode ParseStatement()
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

        private StatementNode ParseVariableDeclarationStatement(int line, int column)
        {
            var variableIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên biến");
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thuộc");
            var typeIdentifier = ParseType();
            ExpressionNode? initialValue = null;
            if (Current()?.Value == "gán")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán"); // will never happen but ok
                initialValue = ParseExpression(0);
            }
            return new VariableDeclarationStatement
            {
                Variable = new VariableNode
                {
                    Name = variableIdentifier.Value,
                    Type = typeIdentifier,
                    Line = variableIdentifier.Line,
                    Column = variableIdentifier.Column
                },
                InitialValue = initialValue,
                Line = line,
                Column = column
            };
        }

        private Types.TypeNode ParseType()
        {
            var (line, column) = GetCurrentPosition();
            string typeIdentifier = "";
            do
            {
                var token = ConsumeOrThrow(TokenType.Identifier, "kiểu dữ liệu");
                typeIdentifier += token.Value;
                if (Current()?.Value != ".") break;
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ".");
                typeIdentifier += ".";
            } while (true);
            return new Types.TypeNode { Name = typeIdentifier, Line = line, Column = column };
        }

        private ExpressionNode ParseExpression(int minPrecedence)
        {
            var (line, column) = GetCurrentPosition();
            ExpressionNode left = ParsePrimary();
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

        private ExpressionNode ParsePrimary()
        {
            var (line, column) = GetCurrentPosition();
            var token = ConsumeOrThrow(TokenType.Literal | TokenType.Identifier | TokenType.Punctuator, "biểu thức");
            switch(token.Type)
            {
                case TokenType.Literal: return new LiteralExpression { Value = token.Value, Line = token.Line, Column = token.Column };
                case TokenType.Identifier: return new VariableExpression { Name = token.Value, Line = token.Line, Column = token.Column };
                case TokenType.Punctuator:
                    if (token.Value == "(")
                    {
                        var expression = ParseExpression(0);
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
                        return expression;
                    }
                    else
                    {
                        UnexpectedHelper(token);
                        throw new(); // never happens
                    }
                default: throw new NotImplementedException();
            }
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

        private Token ConsumeOrThrowIfNotEqual(TokenType expectedTokenTypes, string expected)
        {
            var token = ConsumeOrThrow(expectedTokenTypes, expected);
            if (token.Value != expected) throw new ExpectedException(content, expected, token.Line, token.Column);
            return token;
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
