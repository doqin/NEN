using NEN.Exceptions;
using NEN.Types;
using System.Reflection;

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
            List<FieldDeclarationStatement> fields = [];
            while (Current() != null && Current()!.Value != "kết_thúc")
            {
                var token = Consume();
                switch (token!.Value)
                {
                    case "@":
                        var (left, right) = ParseMarker();
                        if (left != null)
                        {
                            methods.Add(left);
                        }
                        else if (right != null)
                        {
                            fields.Add(right);
                            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
                        }
                        else
                        {
                            throw new("Internal error");
                        }
                        break;
                    case "phương_thức":
                        methods.Add(ParseMethod());
                        break;
                    case "thuộc_tính":
                        fields.Add(ParseFieldDeclarationStatement());
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
                        break;
                    default:
                        UnexpectedHelper(token);
                        break;
                }
            }
            if (Current() == null) OutOfTokenHelper("kết_thúc");
            Consume();
            return new ClassNode { Name = classIdentifier!.Value, Methods = [.. methods], Fields = [..fields], Line = line, Column = column };
        }

        private static readonly string[] markers = [
            "Chính", "Tĩnh", "Công_Khai"
        ];

        private (MethodNode?, FieldDeclarationStatement?) ParseMarker(MethodAttributes methodAttributes = MethodAttributes.Private, FieldAttributes fieldAttributes = FieldAttributes.Private, bool isEntryPoint = false)
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
                case "Công_Khai":
                    if (methodAttributes.HasFlag(MethodAttributes.Public)) throw new RedefinedException(content, "@Công_Khai", line, column);
                    methodAttributes &= ~MethodAttributes.Private;
                    methodAttributes |= MethodAttributes.Public;
                    if (fieldAttributes.HasFlag(FieldAttributes.Public)) throw new RedefinedException(content, "@Công_Khai", line, column);
                    fieldAttributes &= ~FieldAttributes.Private;
                    fieldAttributes |= FieldAttributes.Public;
                    break;
                case "Tĩnh":
                    if (methodAttributes.HasFlag(MethodAttributes.Static)) throw new RedefinedException(content, "@Tĩnh", line, column);
                    methodAttributes |= MethodAttributes.Static;
                    if (fieldAttributes.HasFlag(FieldAttributes.Static)) throw new RedefinedException(content, "@Tĩnh", line, column);
                    fieldAttributes |= FieldAttributes.Static;
                    break;
                default:
                    throw new NotImplementedException();
            }
            var token = ConsumeOrThrow(TokenType.Keyword | TokenType.Marker, "phương_thức");
            switch(token.Value)
            {
                case "@":
                    return ParseMarker(methodAttributes, fieldAttributes, isEntryPoint);
                case "phương_thức":
                    return (ParseMethod(methodAttributes, isEntryPoint), null);
                case "thuộc_tính":
                    return (null, ParseFieldDeclarationStatement(fieldAttributes));
                default:
                    throw new ExpectedException(content, "phương_thức", GetCurrentLine(), GetCurrentColumn());
            }
        }

        private MethodNode ParseMethod(MethodAttributes methodAttributes = MethodAttributes.Private, bool isEntryPoint = false)
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
                parameters.Add(new VariableNode { Name = parameterIdentifier.Value, TypeNode = type, Line = parameterIdentifier.Line, Column = parameterIdentifier.Column });
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
            return new MethodNode { IsEntryPoint = isEntryPoint, MethodAttributes = methodAttributes, MethodName = methodIdentifier.Value, Parameters = [.. parameters], ReturnTypeNode = returnTypeIdentifier, Statements = [.. statements], Line = line, Column = column };
        }

        private FieldDeclarationStatement ParseFieldDeclarationStatement(FieldAttributes fieldAttributes = FieldAttributes.Private)
        {
            var (line, column) = GetCurrentPosition();
            var fieldDeclaration = ParseVariableDeclarationStatement(line, column);
            return new FieldDeclarationStatement
            {
                Line = line,
                Column = column,
                FieldAttributes = fieldAttributes,
                Variable = fieldDeclaration.Variable,
                InitialValue = fieldDeclaration.InitialValue
            };
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
                        case "trả_về":
                            return ParseReturnStatement(line, column);
                        case "nếu":
                            return ParseIfStatement(line, column);
                        default:
                            UnexpectedHelper(token);
                            throw new();
                    }
                default:
                    return ParseExpressionStatementOrAssignmentStatement();
            }
        }

        private IfStatement ParseIfStatement(int line, int column)
        {
            var condition = ParseExpression(0);
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thì");
            List<StatementNode> ifClause = [];
            while(Current() != null && Current()?.Value != "kết_thúc" && Current()?.Value != "không_thì")
            {
                ifClause.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            List<StatementNode> elseClause = [];
            if (Consume()?.Value == "không_thì")
            {
                while (Current() != null && Current()?.Value != "kết_thúc")
                {
                    elseClause.Add(ParseStatement());
                    ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
                    if (elseClause.Count == 1 && elseClause[0].GetType().FullName!.Equals(typeof(IfStatement).FullName))
                    {
                        SetBack(); SetBack();
                    }
                }
            }
            else SetBack();
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "kết_thúc");
            return new IfStatement { 
                Condition = condition,
                IfClause = [..ifClause],
                ElseClause = [..elseClause],
                Line = line,
                Column = column
            };
        }

        private ReturnStatement ParseReturnStatement(int line, int column)
        {
            if (Current()?.Value == ";") 
                return new ReturnStatement { 
                    Line = line, 
                    Column = column 
                };
            var expression = ParseExpression(0);
            return new ReturnStatement { 
                Expression = expression, 
                Line = line, 
                Column = column 
            };
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

        private VariableDeclarationStatement ParseVariableDeclarationStatement(int line, int column)
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
                    TypeNode = typeIdentifier,
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
                        expression = new AmbiguousMethodCallExpression { MethodName = token.Value, Arguments = arguments, Line = line, Column = column };
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
            while (Current()?.Value == "." || Current()?.Value == "[")
            {
                if (Current()?.Value == ".")
                {
                    expression = ParseStandardMethodCallOrFieldAccessmentExpression(expression!);
                }
                else
                {
                    expression = ParseArrayIndexingExpression(expression!);
                }
            }
            return expression!;
        }

        private ExpressionNode ParseStandardMethodCallOrFieldAccessmentExpression(ExpressionNode objectNode)
        {
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ".");
            var (line, column) = GetCurrentPosition();
            var identifier = ConsumeOrThrow(TokenType.Identifier, "tên thuộc tính/tên phương thức");
            if (Current() != null && Current()?.Value != "(")
            {
                return new StandardFieldAccessmentExpression { 
                    Object = objectNode, 
                    FieldName = identifier.Value, 
                    Line = line, 
                    Column = column 
                };
            }
            var arguments = ParseArguments();
            return new StandardMethodCallExpression { 
                Object = objectNode, 
                MethodName = identifier.Value, 
                Arguments = arguments, 
                Line = line, 
                Column = column 
            };
        }

        private ArrayIndexingExpression ParseArrayIndexingExpression(ExpressionNode objectNode)
        {
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
            var (line, column) = GetCurrentPosition();
            var indexNode = ParseExpression(0);
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
            return new ArrayIndexingExpression { Array = objectNode, Index = indexNode, Line = line, Column = column };
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
                    ElementTypeNode = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[]",
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
                        ReturnTypeNode = arrayType, 
                        Size = size, 
                        Elements = [..elements], 
                        Line = line, 
                        Column = column 
                    };
                case NamedType namedType:
                    List<AssignmentStatement> assignments = [];
                    if (Current()?.Value == "{")
                    {
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "{");
                        while (Current() != null && Current()?.Value != "}")
                        {
                            var (assignmentLine, assignmentColumn) = GetCurrentPosition();
                            var fieldIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên thuộc tính");
                            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán");
                            var value = ParseExpression(0);
                            assignments.Add(new AssignmentStatement { 
                                Destination = new StandardFieldAccessmentExpression {
                                    ReturnTypeNode = namedType,
                                    Object = new DuplicateExpression { ReturnTypeNode = namedType, Line = assignmentLine, Column = assignmentColumn },
                                    FieldName = fieldIdentifier.Value,
                                    Line = assignmentLine,
                                    Column = assignmentColumn
                                },
                                Source = value,
                                Line = assignmentLine,
                                Column = assignmentColumn
                            });
                            if (Current() != null && Current()?.Value != "}") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                        }
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "}");
                    }
                    return new NewObjectExpression { 
                        ReturnTypeNode = namedType, 
                        FieldInitializations = [..assignments] , 
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
                    ElementTypeNode = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[]",
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
                return new StaticFieldAccessmentExpression
                {
                    TypeNode = type,
                    FieldName = identifier[^1],
                    Line = line,
                    Column = column
                };
            }
            var arguments = ParseArguments();
            return new StaticMethodCallExpression
            {
                TypeNode = type,
                Arguments = arguments,
                MethodName = identifier[^1],
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
            { "hoặc", 1 },
            { "và", 2 },
            { "=", 3 },
            { "!=", 3 },
            { "<=", 4 },
            { "<", 4 },
            { ">=", 4 },
            { ">", 4 },
            { "+", 5 },
            { "-", 5 },
            { "*", 6 },
            { "/", 6 },
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
            if (index >= tokens.Length)
            {
                return (-1, -1);
            }
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
