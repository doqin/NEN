using NEN.Exceptions;
using NEN.AST;
using static NEN.Lexer;
using System.Reflection;
using System.Reflection.Emit;


namespace NEN
{
    public class Parser(string moduleName, string[] contentLines, Token[] tokens)
    {
        private readonly string moduleName = moduleName;
        private readonly Token[] tokens = tokens;
        private int index = 0;
        private readonly string[] content = contentLines;

        /// <summary>
        /// Parses the current token stream and constructs a module part representing the source file, including its
        /// classes and using namespace statements.
        /// </summary>
        /// <returns>A <see cref="ModulePart"/> object containing the parsed classes and using namespace statements from the
        /// source file.</returns>
        /// <exception cref="ExpectedException">Thrown if the token stream does not match the expected module structure, such as when a class declaration is
        /// missing or an unexpected token is encountered.</exception>
        public ModulePart Parse()
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
                                var (namespaceIdentifiers, startLine, startColumn, endLine, endColumn) = ParseIdentifier();
                                usingNamespaceStatements.Add(
                                    new UsingNamespaceStatement {
                                        Namespace = namespaceIdentifiers,
                                        StartLine = startLine,
                                        StartColumn = startColumn,
                                        EndLine = endLine,
                                        EndColumn = endColumn,
                                    }
                                );
                                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
                                break;
                            case "lớp":
                                classes.Add(ParseClass());
                                break;
                            default:
                                throw new ExpectedException(content, "lớp", token.StartLine, token.StartColumn, token.EndLine, token.EndColumn);
                        }
                        break;
                    default:
                        throw new ExpectedException(content, "lớp", token.StartLine, token.StartColumn, token.EndLine, token.EndColumn);
                }
            }
            return new ModulePart { 
                SourceName = moduleName, 
                Source = content,
                Classes = [.. classes], 
                UsingNamespaces = [.. usingNamespaceStatements.Distinct()] 
            };
        }

        private ClassNode ParseClass()
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
            var classIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên lớp");
            var (endLine, endColumn) = GetPreviousEndPosition();
            List<ConstructorNode> constructors = [];
            List<MethodNode> methods = [];
            List<FieldDeclarationStatement> fields = [];
            while (Current(out var end) && end?.Value != "kết_thúc")
            {
                var token = Consume();
                switch (token!.Value)
                {
                    case "@":
                        var (left, right) = ParseMarker(classIdentifier.Value);
                        if (left != null)
                        {
                            switch(left)
                            {
                                case MethodNode method:
                                    methods.Add(method); break;
                                case ConstructorNode constructor:
                                    constructors.Add(constructor); break;
                                default:
                                    throw new NotImplementedException();
                            }
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
                    case "phương_thức_khởi_tạo":
                        constructors.Add(ParseConstructor(classIdentifier.Value));
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
            if (!Current(out _)) OutOfTokenHelper("kết_thúc");
            Consume();
            return new ClassNode { 
                Name = classIdentifier!.Value, 
                Constructors = [..constructors],
                Methods = [.. methods], 
                Fields = [..fields], 
                StartLine = startLine, 
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn 
            };
        }

        private static readonly string[] markers = [
            "Chính", "Tĩnh", "Công_Khai"
        ];

        private (AST.MethodBase?, FieldDeclarationStatement?) ParseMarker(string className, MethodAttributes methodAttributes = MethodAttributes.Private, FieldAttributes fieldAttributes = FieldAttributes.Private, bool isEntryPoint = false)
        {
            var annotationIdentifier = ConsumeOrThrow(TokenType.Identifier, "thuộc tính phương thức");
            if (!markers.Contains(annotationIdentifier.Value)) throw new ExpectedException(content, "thuộc tính phương thức", annotationIdentifier.StartLine, annotationIdentifier.StartColumn, annotationIdentifier.EndLine, annotationIdentifier.EndColumn);
            switch(annotationIdentifier.Value)
            {
                case "Chính":
                    if (isEntryPoint == true) throw new RedefinedException(content, "@Chính", annotationIdentifier.StartLine, annotationIdentifier.StartColumn, annotationIdentifier.EndLine, annotationIdentifier.EndColumn);
                    isEntryPoint = true;
                    break;
                case "Công_Khai":
                    if (methodAttributes.HasFlag(MethodAttributes.Public)) throw new RedefinedException(
                        content, 
                        "@Công_Khai", 
                        annotationIdentifier.StartLine, 
                        annotationIdentifier.StartColumn, 
                        annotationIdentifier.EndLine, 
                        annotationIdentifier.EndColumn
                        );
                    methodAttributes &= ~MethodAttributes.Private;
                    methodAttributes |= MethodAttributes.Public;
                    if (fieldAttributes.HasFlag(FieldAttributes.Public)) throw new RedefinedException(
                        content, 
                        "@Công_Khai", 
                        annotationIdentifier.StartLine, 
                        annotationIdentifier.StartColumn, 
                        annotationIdentifier.EndLine, 
                        annotationIdentifier.EndColumn
                        );
                    fieldAttributes &= ~FieldAttributes.Private;
                    fieldAttributes |= FieldAttributes.Public;
                    break;
                case "Tĩnh":
                    if (methodAttributes.HasFlag(MethodAttributes.Static)) throw new RedefinedException(
                        content, 
                        "@Tĩnh", 
                        annotationIdentifier.StartLine, 
                        annotationIdentifier.StartColumn, 
                        annotationIdentifier.EndLine, 
                        annotationIdentifier.EndColumn
                        );
                    methodAttributes |= MethodAttributes.Static;
                    if (fieldAttributes.HasFlag(FieldAttributes.Static)) throw new RedefinedException(
                        content, 
                        "@Tĩnh", 
                        annotationIdentifier.StartLine, 
                        annotationIdentifier.StartColumn, 
                        annotationIdentifier.EndLine, 
                        annotationIdentifier.EndColumn
                        );
                    fieldAttributes |= FieldAttributes.Static;
                    break;
                default:
                    throw new NotImplementedException();
            }
            var token = ConsumeOrThrow(TokenType.Keyword | TokenType.Marker, "phương_thức");
            switch(token.Value)
            {
                case "@":
                    return ParseMarker(className, methodAttributes, fieldAttributes, isEntryPoint);
                case "phương_thức":
                    return (ParseMethod(methodAttributes, isEntryPoint), null);
                case "phương_thức_khởi_tạo":
                    return (ParseConstructor(className, methodAttributes), null);
                case "thuộc_tính":
                    return (null, ParseFieldDeclarationStatement(fieldAttributes));
                default:
                    var (el, ec) = GetCurrentEndPosition();
                    throw new ExpectedException(
                        content, 
                        "phương_thức", 
                        GetCurrentLine(), 
                        GetCurrentColumn(), 
                        el, 
                        ec
                        );
            }
        }

        private ConstructorNode ParseConstructor(string className, MethodAttributes constructorAttributes = MethodAttributes.Private)
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
            var (endLine, endColumn) = GetCurrentEndPosition();
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "(");
            List<VariableNode> parameters = [];
            while (Current(out var rParen) && rParen!.Value != ")")
            {
                var parameterIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên tham số");
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thuộc");
                var type = ParseType();
                parameters.Add(new VariableNode
                {
                    Name = parameterIdentifier.Value,
                    TypeNode = type,
                    StartLine = parameterIdentifier.StartLine,
                    StartColumn = parameterIdentifier.StartColumn,
                    EndLine = parameterIdentifier.EndLine,
                    EndColumn = parameterIdentifier.EndColumn
                });
                if (Current(out rParen) && rParen!.Value != ")") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
            }
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
            List<StatementNode> statements = [];
            while (Current(out var token) && token!.Value != "kết_thúc")
            {
                statements.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            if (!Current(out _)) OutOfTokenHelper("kết_thúc");
            Consume();
            return new ConstructorNode
            {
                MethodAttributes = constructorAttributes,
                Parameters = [..parameters],
                Statements = [..statements],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn,
                DeclaringTypeNode = new NamedType { 
                    Namespaces = [],
                    Name = className,
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn
                }
            };
        }

        private MethodNode ParseMethod(MethodAttributes methodAttributes = MethodAttributes.Private, bool isEntryPoint = false)
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
            var methodIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên phương thức");
            var (endLine, endColumn) = GetPreviousEndPosition();
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "(");
            List<VariableNode> parameters = [];
            while (Current(out var rParen) && rParen!.Value != ")")
            {
                var parameterIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên tham số");
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thuộc");
                var type = ParseType();
                parameters.Add(new VariableNode { 
                    Name = parameterIdentifier.Value, 
                    TypeNode = type, 
                    StartLine = parameterIdentifier.StartLine, 
                    StartColumn = parameterIdentifier.StartColumn, 
                    EndLine = parameterIdentifier.EndLine, 
                    EndColumn = parameterIdentifier.EndColumn 
                });
                if (Current(out rParen) && rParen!.Value != ")") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
            }
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
            ConsumeOrThrowIfNotEqual(TokenType.Operator, "->");
            var returnTypeIdentifier = ParseType();
            List<StatementNode> statements = [];
            while (Current(out var token) && token!.Value != "kết_thúc")
            {
                statements.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            if (!Current(out _)) OutOfTokenHelper("kết_thúc");
            Consume();
            return new MethodNode { 
                IsEntryPoint = isEntryPoint, 
                MethodAttributes = methodAttributes, 
                MethodName = methodIdentifier.Value, 
                Parameters = [.. parameters], 
                ReturnTypeNode = returnTypeIdentifier, 
                Statements = [.. statements], 
                StartLine = startLine, 
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn 
            };
        }

        private FieldDeclarationStatement ParseFieldDeclarationStatement(FieldAttributes fieldAttributes = FieldAttributes.Private)
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
            var fieldDeclaration = ParseVariableDeclarationStatement(startLine, startColumn);
            var (endLine, endColumn) = GetPreviousEndPosition();
            return new FieldDeclarationStatement
            {
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = fieldDeclaration.Variable.EndLine,
                EndColumn = fieldDeclaration.Variable.EndColumn,
                FieldAttributes = fieldAttributes,
                Variable = fieldDeclaration.Variable,
                InitialValue = fieldDeclaration.InitialValue
            };
        }

        private StatementNode ParseStatement()
        {
            var (line, column) = GetCurrentStartPosition();
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
                        case "trong_khi":
                            return ParseWhileStatement(line, column);
                        case "thoát":
                            return new BreakStatement { 
                                StartLine = token.StartLine, 
                                StartColumn = token.StartColumn, 
                                EndLine = token.EndLine, 
                                EndColumn = token.EndColumn
                            };
                        default:
                            UnexpectedHelper(token);
                            throw new();
                    }
                default:
                    return ParseExpressionStatementOrAssignmentStatement();
            }
        }

        private StatementNode ParseWhileStatement(int startLine, int startColumn)
        {
            var condition = ParseExpression(0);
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thì");
            List<StatementNode> body = [];
            while (Current(out var token) && token?.Value != "kết_thúc")
            {
                body.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "kết_thúc");
            var (endLine, endColumn) = GetPreviousEndPosition();
            return new WhileStatement
            {
                Condition = condition,
                Body = [.. body],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
        }

        private IfStatement ParseIfStatement(int startLine, int startColumn)
        {
            var condition = ParseExpression(0);
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thì");
            List<StatementNode> ifClause = [];
            while(Current(out var token) && token?.Value != "kết_thúc" && token?.Value != "không_thì")
            {
                ifClause.Add(ParseStatement());
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ";");
            }
            List<StatementNode> elseClause = [];
            if (Consume()?.Value == "không_thì")
            {
                while (Current(out var end) && end?.Value != "kết_thúc")
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
            var (endLine, endColumn) = GetPreviousEndPosition();
            return new IfStatement { 
                Condition = condition,
                IfClause = [..ifClause],
                ElseClause = [..elseClause],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
        }

        private ReturnStatement ParseReturnStatement(int startLine, int startColumn)
        {
            if (Current(out var semiColon) && semiColon?.Value == ";")
            {
                var (endLine_, endColumn_) = GetPreviousEndPosition();
                return new ReturnStatement
                {
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endLine_,
                    EndColumn = endColumn_
                };
            }
                
            var expression = ParseExpression(0);
            var (endLine, endColumn) = GetPreviousEndPosition();
            return new ReturnStatement { 
                Expression = expression, 
                StartLine = startLine, 
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn 
            };
        }

        private StatementNode ParseExpressionStatementOrAssignmentStatement()
        {
            SetBack();
            var (startLine, startColumn) = GetCurrentStartPosition();
            var dest = ParsePrimary();
            if (Current(out var assign) && assign?.Value == "gán")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán");
                var src = ParseExpression(0);
                var (endLine, endColumn) = GetPreviousEndPosition();
                return new AssignmentStatement { 
                    Destination = dest, 
                    Source = src, 
                    StartLine = startLine, 
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn 
                };
            }
            else
            {
                var (endLine, endColumn) = GetPreviousEndPosition();
                return new ExpressionStatement { 
                    Expression = dest, 
                    StartLine = startLine, 
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn 
                };
            }
        }

        private ExpressionNode[] ParseArguments()
        {
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "(");
            List<ExpressionNode> arguments = [];
            while (Current(out var rParen) && rParen?.Value != ")")
            {
                var argumentExpression = ParseExpression(0);
                if (Current(out var token) && token?.Value != ")") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                arguments.Add(argumentExpression);
            }
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ")");
            return [.. arguments];
        }

        private VariableDeclarationStatement ParseVariableDeclarationStatement(int startLine, int startColumn)
        {
            var variableIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên biến");
            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "thuộc");
            var typeIdentifier = ParseType();
            ExpressionNode? initialValue = null;
            if (Current(out var assign) && assign?.Value == "gán")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán"); // will never happen but ok
                initialValue = ParseExpression(0);
            }
            var (endLine, endColumn) = GetPreviousEndPosition();
            return new VariableDeclarationStatement
            {
                Variable = new VariableNode
                {
                    Name = variableIdentifier.Value,
                    TypeNode = typeIdentifier,
                    StartLine = variableIdentifier.StartLine,
                    StartColumn = variableIdentifier.StartColumn,
                    EndLine = variableIdentifier.EndLine,
                    EndColumn = variableIdentifier.EndColumn
                },
                InitialValue = initialValue,
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
        }

        private (string[], int, int, int, int) ParseIdentifier()
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
            List<string> identifiers = [];
            do
            {
                var token = ConsumeOrThrow(TokenType.Identifier, "tên không gian");
                identifiers.Add(token.Value);
                if (Current(out var ns) && ns?.Value != "::") break;
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "::");
            } while (true);
            var (endLine, endColumn) = GetPreviousEndPosition();
            return ([.. identifiers], startLine, startColumn, endLine, endColumn);
        }

        private ExpressionNode ParseExpression(int minPrecedence)
        {
            var (startLine, startColumn) = GetCurrentStartPosition();
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
                var (endLine, endColumn) = GetPreviousEndPosition();
                left = new BinaryExpression { 
                    Left = left, 
                    Operator = op.Value, 
                    Right = right, 
                    StartLine = startLine, 
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn 
                };
            }
            return left;
        }

        private ExpressionNode ParsePrimary()
        {
            ExpressionNode? expression = null;
            var (startLine, startColumn) = GetCurrentStartPosition();
            var token = ConsumeOrThrow(TokenType.Literal | TokenType.Identifier | TokenType.Punctuator | TokenType.Keyword, "biểu thức");
            switch (token.Type)
            {
                case TokenType.Literal:
                    expression = new LiteralExpression { 
                        Value = token.Value, 
                        StartLine = token.StartLine, 
                        StartColumn = token.StartColumn,
                        EndLine = token.EndLine,
                        EndColumn = token.EndColumn 
                    };
                    break;
                case TokenType.Identifier:
                    if (Current(out var lParen) && lParen?.Value == "(")
                    {
                        var (endLine, endColumn) = GetPreviousEndPosition();
                        var arguments = ParseArguments();
                        expression = new AmbiguousMethodCallExpression { 
                            MethodName = token.Value, 
                            Arguments = arguments, 
                            StartLine = startLine, 
                            StartColumn = startColumn,
                            EndLine = endLine,
                            EndColumn = endColumn 
                        };
                    }
                    else if (Current(out var ns) && ns?.Value == "::")
                    {
                        SetBack();
                        expression = ParseStaticAccessment();
                    }
                    else
                    {
                        expression = new VariableExpression { 
                            Name = token.Value, 
                            StartLine = token.StartLine, 
                            StartColumn = token.StartColumn,
                            EndLine = token.EndLine,
                            EndColumn = token.EndColumn 
                        };
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
            while (Current(out var t) && (t?.Value == "." || t?.Value == "["))
            {
                if (Current(out var dot) && dot?.Value == ".")
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
            var (startLine, startColumn) = GetCurrentStartPosition();
            var identifier = ConsumeOrThrow(TokenType.Identifier, "tên thuộc tính/tên phương thức");
            var (endLine, endColumn) = GetPreviousEndPosition();
            if (Current(out var token) && token?.Value != "(")
            {
                return new StandardFieldAccessmentExpression { 
                    Object = objectNode, 
                    FieldName = identifier.Value, 
                    StartLine = startLine, 
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn
                };
            }
            var arguments = ParseArguments();
            return new StandardMethodCallExpression { 
                Object = objectNode, 
                MethodName = identifier.Value, 
                Arguments = arguments, 
                StartLine = startLine, 
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn 
            };
        }

        private ArrayIndexingExpression ParseArrayIndexingExpression(ExpressionNode objectNode)
        {
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
            var (line, column) = GetCurrentStartPosition();
            var indexNode = ParseExpression(0);
            ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
            return new ArrayIndexingExpression { 
                Array = objectNode, 
                Index = indexNode, 
                StartLine = line, 
                StartColumn = column,
                EndLine = line,
                EndColumn = column 
            };
        }

        private ExpressionNode ParseNewExpression()
        {
            var (typeIdentifiers, startLine, startColumn, endLine, endColumn) = ParseIdentifier();
            TypeNode type = new NamedType
            {
                Namespaces = typeIdentifiers[0..^1],
                Name = typeIdentifiers[^1],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
            int nesting = 1;
            ExpressionNode? size = null;
            while (Current(out var lBrack) && lBrack?.Value == "[")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
                if (nesting == 1 && Current(out var token) && token?.Value != "]")
                {
                    size = ParseExpression(0);
                }
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
                type = new ArrayType
                {
                    ElementTypeNode = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[]",
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn
                };
                nesting++;
            }
            switch(type)
            {
                case ArrayType arrayType:
                    List<ExpressionNode> elements = [];
                    if (Current(out var lBrace) && lBrace?.Value == "{")
                    {
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "{");
                        while (Current(out var token) && token?.Value != "}")
                        {
                            elements.Add(ParseExpression(0));
                            if (Current(out var rBrace) && rBrace?.Value != "}") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                        }
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "}");
                    }
                    return new NewArrayExpression { 
                        ReturnTypeNode = arrayType, 
                        Size = size, 
                        Elements = [..elements], 
                        StartLine = startLine, 
                        StartColumn = startColumn,
                        EndLine = endLine,
                        EndColumn = endColumn 
                    };
                case NamedType namedType:
                    List<AssignmentStatement> assignments = [];
                    if (Current(out lBrace) && lBrace?.Value == "{")
                    {
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "{");
                        while (Current(out var rBrace) && rBrace?.Value != "}")
                        {
                            var (startAssignmentLine, startAssignmentColumn) = GetCurrentStartPosition();
                            var fieldIdentifier = ConsumeOrThrow(TokenType.Identifier, "tên thuộc tính");
                            var (endFieldLine, endFieldColumn) = GetPreviousEndPosition();
                            ConsumeOrThrowIfNotEqual(TokenType.Keyword, "gán");
                            var value = ParseExpression(0);
                            var (endAssignmentLine, endAssignmentColumn) = GetPreviousEndPosition();
                            assignments.Add(new AssignmentStatement { 
                                Destination = new StandardFieldAccessmentExpression {
                                    ReturnTypeNode = namedType,
                                    Object = new DuplicateExpression { 
                                        ReturnTypeNode = namedType, 
                                        StartLine = startAssignmentLine, 
                                        StartColumn = startAssignmentColumn,
                                        EndLine = endFieldLine,
                                        EndColumn = endFieldColumn
                                    },
                                    FieldName = fieldIdentifier.Value,
                                    StartLine = startAssignmentLine,
                                    StartColumn = startAssignmentColumn,
                                    EndLine = endFieldLine,
                                    EndColumn = endFieldColumn
                                },
                                Source = value,
                                StartLine = startAssignmentLine,
                                StartColumn = startAssignmentColumn,
                                EndLine = endAssignmentLine,
                                EndColumn = endAssignmentColumn
                            });
                            if (Current(out var token) && token?.Value != "}") ConsumeOrThrowIfNotEqual(TokenType.Punctuator, ",");
                        }
                        ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "}");
                    }
                    return new InlineConstructionExpression { 
                        ReturnTypeNode = namedType, 
                        FieldInitializations = [..assignments] , 
                        StartLine = startLine, 
                        StartColumn = startColumn,
                        EndLine = endLine,
                        EndColumn = endColumn 
                    };
                default:
                    throw new NotImplementedException();
            }
        }

        private TypeNode ParseType()
        {
            var (typeIdentifiers, startLine, startColumn, endLine, endColumn) = ParseIdentifier();
            TypeNode type = new NamedType
            {
                Namespaces = typeIdentifiers[0..^1],
                Name = typeIdentifiers[^1],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
            while (Current(out var token) && token?.Value == "[")
            {
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "[");
                ConsumeOrThrowIfNotEqual(TokenType.Punctuator, "]");
                type = new ArrayType
                {
                    ElementTypeNode = type,
                    Namespaces = type.Namespaces,
                    Name = $"{type.Name}[]",
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn
                };
            }
            return type;
        }

        private ExpressionNode ParseStaticAccessment()
        {
            var (identifier, startLine, startColumn, endLine, endColumn) = ParseIdentifier();
            NamedType type = new()
            {
                Namespaces = identifier[0..^2],
                Name = identifier[^2],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
            if (Current(out var token) && token?.Value != "(")
            {
                return new StaticFieldAccessmentExpression
                {
                    TypeNode = type,
                    FieldName = identifier[^1],
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endColumn,
                    EndColumn = endColumn
                };
            }
            var arguments = ParseArguments();
            return new StaticMethodCallExpression
            {
                TypeNode = type,
                Arguments = arguments,
                MethodName = identifier[^1],
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
            };
        }

        /* Helpers */

        private void UnexpectedHelper(Token token)
        {
            throw new UnexpectedException(content, token.Value, token.StartLine, token.StartColumn, token.EndLine, token.EndColumn);
        }

        private Token ConsumeOrThrow(TokenType expectedTokenTypes, string expected)
        {
            var token = Consume();
            if (token == null) OutOfTokenHelper(expected);
            else if (!expectedTokenTypes.HasFlag(token.Type)) throw new ExpectedException(content, expected, token.StartLine, token.StartColumn, token.EndLine, token.EndColumn);
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
            if (token.Value != expected) throw new ExpectedException(content, expected, token.StartLine, token.StartColumn, token.EndLine, token.EndColumn);
            return token;
        }

        private void OutOfTokenHelper(string expected)
        {
            throw new ExpectedException(
                content,
                expected,
                GetCurrentLine(),
                GetCurrentColumn() + GetCurrentLength() + 1,
                GetCurrentLine(),
                GetCurrentColumn() + GetCurrentLength() + 1
            );
        }

        private bool Current(out Token? token)
        {
            if (index < tokens.Length)
            {
                token = tokens[index];
                return true;
            }
            else
            {
                token = null;
                return false;
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
            if (Current(out var token))
            {
                if (token?.Type == TokenType.Operator || token?.Type == TokenType.Keyword)
                {
                    if (precedences.TryGetValue(token.Value, out int value))
                    {
                        return value;
                    }
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
            if (index < tokens.Length)
            {
                return tokens[index].StartLine;
            }
            return tokens.Last().EndLine; // meh
        }
        private int GetCurrentColumn()
        {
            if (index < tokens.Length)
            {
                return tokens[index].StartColumn;
            }
            return tokens.Last().EndColumn + 2;
        }
        private int GetCurrentLength()
        {
            if (index < tokens.Length)
            {
                return tokens[index].Value.Length;
            }
            return 1;
        }
        private (int, int) GetCurrentEndPosition()
        {
            if (index >= tokens.Length)
            {
                return (-1, -1);
            }
            return (tokens[index].EndLine, tokens[index].EndColumn);
        }
        private (int, int) GetPreviousEndPosition()
        {
            if (index - 1 < 0)
            {
                return (-1, -1);
            }
            return (tokens[index - 1].EndLine, tokens[index - 1].EndColumn);
        }

        private (int, int) GetCurrentStartPosition()
        {
            if (index >= tokens.Length)
            {
                return (-1, -1);
            }
            return (tokens[index].StartLine, tokens[index].StartColumn);
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
