using NEN.Exceptions;
using NEN.Types;
using System.Reflection;
using System.Runtime.CompilerServices;

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
            List<UsingNamespaceStatement> usingNamespaceStatements = [];
            while (index < tokens.Length)
            {
                var token = Consume();
                switch (token!.Type)
                {
                    case TokenType.Keyword:
                        switch (token.Value)
                        {
                            case "sử_dụng":
                                var (namespaceIdentifiers, line, column) = ParseIdentifier();
                                usingNamespaceStatements.Add(
                                    new UsingNamespaceStatement {
                                        Namespace = namespaceIdentifiers,
                                        Line = line,
                                        Column = column,
                                    }
                                );
                                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
                                break;
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
            return new Types.Module { 
                Name = moduleName, 
                Classes = [.. classes], 
                UsingNamespaces = [.. usingNamespaceStatements.Distinct()] 
            };
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

        private static readonly string[] markers = [
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
            switch (token!.Type)
            {
                case TokenType.Keyword:
                    switch (token!.Value)
                    {
                        case "biến":
                            return ParseVariableDeclarationStatement(line, column);
                        default:
                            UnexpectedHelper(token);
                            throw new();
                    }
                default:
                    return ParseExpressionStatementOrAssignmentStatement();
            }
        }

        private StatementNode ParseExpressionStatementOrAssignmentStatement()
        {
            SetBack();
            var (line, column) = GetCurrentPosition();
            var dest = ParsePrimary();
            if (Current()?.Value == "gán")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán");
                var src = ParseExpression(0);
                return new AssignmentStatement { Destination = dest, Source = src, Line = line, Column = column };
            }
            else
            {
                return new ExpressionStatement { Expression = dest, Line = line, Column = column };
            }
        }

        private ExpressionNode[] ParseArguments()
        {
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "(");
            List<ExpressionNode> arguments = [];
            while (Current() != null && Current()?.Value != ")")
            {
                var argumentExpression = ParseExpression(0);
                if (Current() != null && Current()!.Value != ")") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                arguments.Add(argumentExpression);
            }
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
            return [.. arguments];
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

        private (string[], int, int) ParseIdentifier()
        {
            var (line, column) = GetCurrentPosition();
            List<string> identifiers = [];
            do
            {
                var token = ConsumeOrThrow(TokenType.Identifier, "tên không gian");
                identifiers.Add(token.Value);
                if (Current()?.Value != "::") break;
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "::");
            } while (true);
            return ([.. identifiers], line, column);
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
            ExpressionNode? expression = null;
            var (line, column) = GetCurrentPosition();
            var token = ConsumeOrThrow(TokenType.Literal | TokenType.Identifier | TokenType.Punctuator | TokenType.Keyword, "biểu thức");
            switch (token.Type)
            {
                case TokenType.Literal:
                    expression = new LiteralExpression { Value = token.Value, Line = token.Line, Column = token.Column }; 
                    break;
                case TokenType.Identifier: 
                    if (Current()?.Value == "(")
                    {
                        var arguments = ParseArguments();
                        expression = new AmbiguousMethodCallExpression { Name = token.Value, Arguments = arguments, Line = line, Column = column };
                    }
                    else if (Current()?.Value == "::")
                    {
                        SetBack();
                        expression = ParseStaticAccessment();
                    }
                    else
                    {
                        expression = new VariableExpression { Name = token.Value, Line = token.Line, Column = token.Column };
                    }
                    break;
                case TokenType.Punctuator:
                    if (token.Value == "(")
                    {
                        expression = ParseExpression(0);
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
                    }
                    else
                    {
                        UnexpectedHelper(token);
                    }
                    break;
                case TokenType.Keyword:
                    if (token.Value == "tạo")
                    {
                        expression = ParseNewExpression();
                    }
                    else
                    {
                        UnexpectedHelper(token);
                    }
                    break;
                default: throw new NotImplementedException();
            }
            while (Current()?.Value == ".")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ".");
                var identifier = ConsumeOrThrow(TokenType.Identifier, "tên thuộc tính/tên phương thức");
                if (Current() != null && Current()?.Value != "(")
                {
                    throw new NotImplementedException();
                }
                var arguments = ParseArguments();
                expression = new StandardMethodCallExpression { Object = expression!, Name = identifier.Value, Arguments = arguments, Line = line, Column = column };
            }
            return expression!;
        }

        private ExpressionNode ParseNewExpression()
        {
            var (typeIdentifiers, line, column) = ParseIdentifier();
            TypeNode type = new NamedType
            {
                Namespaces = typeIdentifiers[0..^1],
                Name = typeIdentifiers[^1],
                Line = line,
                Column = column
            };
            int nesting = 1;
            ExpressionNode? size = null;
            while (Current()?.Value == "[")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
                if (nesting == 1 && Current() != null && Current()?.Value != "]")
                {
                    size = ParseExpression(0);
                }
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
                type = new ArrayType
                {
                    ElementType = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[*]",
                    Line = line,
                    Column = column
                };
                nesting++;
            }
            switch(type)
            {
                case ArrayType arrayType:
                    List<ExpressionNode> elements = [];
                    if (Current()?.Value == "{")
                    {
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "{");
                        while (Current() != null && Current()?.Value != "}")
                        {
                            elements.Add(ParseExpression(0));
                            if (Current() != null && Current()?.Value != "}") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                        }
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "}");
                    }
                    return new NewArrayExpression { 
                        ReturnType = arrayType, 
                        Size = size, 
                        Elements = [..elements], 
                        Line = line, 
                        Column = column 
                    };
                default:
                    throw new NotImplementedException();
            }
        }

        private TypeNode ParseType()
        {
            var (typeIdentifiers, line, column) = ParseIdentifier();
            TypeNode type = new NamedType
            {
                Namespaces = typeIdentifiers[0..^1],
                Name = typeIdentifiers[^1],
                Line = line,
                Column = column
            };
            while (Current()?.Value == "[")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
                type = new ArrayType
                {
                    ElementType = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[*]",
                    Line = line,
                    Column = column
                };
            }
            return type;
        }

        private ExpressionNode ParseStaticAccessment()
        {
            var (identifier, line, column) = ParseIdentifier();
            NamedType type = new()
            {
                Namespaces = identifier[0..^2],
                Name = identifier[^2],
                Line = line,
                Column = column
            };
            if (Current() != null && Current()?.Value != "(")
            {
                throw new NotImplementedException();
            }
            var arguments = ParseArguments();
            return new StaticMethodCallExpression
            {
                Type = type,
                Arguments = arguments,
                Name = identifier[^1],
                Line = line,
                Column = column
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

        /// <summary>
        /// Consumes the next token if it matches the specified type and value; otherwise, throws an exception.
        /// </summary>
        /// <param name="expectedTokenTypes">The expected token type or combination of token types to match against the next token.</param>
        /// <param name="expected">The expected string value of the token. The token must have this value to be considered a match.</param>
        /// <returns>The consumed token that matches the specified type and value.</returns>
        /// <exception cref="ExpectedException">Thrown if the next token does not match the specified type or its value does not equal the expected value.</exception>
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
        private bool SetBack()
        {
            index--;
            if (index < 0) return false;
            return true;
        }
        private int GetCurrentIndex()
        {
            return index;
        }
    }
}
