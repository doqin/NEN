using NEN.Exceptions;
using NEN.Types;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace NEN
{
    public class StaticAnalyzer(string[] contentLines, Types.Module module, string assemblyName, string[] assemblyPaths)
    {
        private readonly string[] content = contentLines;
        private readonly Dictionary<string, Type> typeTable = [];
        private readonly Dictionary<(string, Type[]), MethodInfo> moduleMethods = new(new MethodSignatureComparer());
        private readonly Types.Module module = module;

        public void Analyze()
        {
            SetupAssembly();
            foreach (var usingNamespaceStatement in module.UsingNamespaces)
            {
                AnalyzeUsingNamespaceStatement(usingNamespaceStatement);
            }
            // Define every class in the module
            foreach (var c in module.Classes)
            {
                DefineClass(c);
            }
            // Define every module in the classes
            List<SymbolTable<TypeNode>[]> lsLsSt = [];
            foreach (var c in module.Classes)
            {
                List<SymbolTable<TypeNode>> lsSt = [];
                foreach (var method in c.Methods)
                {
                    lsSt.Add(DefineMethod(c, method));
                }
                lsLsSt.Add([.. lsSt]);
            }
            // Analyze its the method bodies in each class
            for (int i = 0; i < module.Classes.Length; i++)
            {
                AnalyzeClass(module.Classes[i], lsLsSt[i]);
            }
        }

        private void SetupAssembly()
        {
            string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            PathAssemblyResolver resolver = new([.. Directory.GetFiles(runtimePath, "*.dll"), .. assemblyPaths]);
            module.MetadataLoadContext = new(resolver);
            module.CoreAssembly = module.MetadataLoadContext.CoreAssembly!;
            module.AssemblyBuilder = new(new AssemblyName(assemblyName), module.CoreAssembly!);
            module.ModuleBuilder = module.AssemblyBuilder.DefineDynamicModule(module.Name);
            module.AvailableNamespaces =
                [.. module.MetadataLoadContext
                    .GetAssemblies()
                    .Select(
                        assembly => assembly.GetExportedTypes()
                            .Select(type => type.Namespace)
                    ).Aggregate(
                        new List<string>(), 
                        (x, y) => x.Concat(y).Distinct().ToList()!)];
        }

        private void DefineClass(ClassNode c)
        {
            c.TypeBuilder = module.ModuleBuilder!.DefineType(
                c.Name,
                TypeAttributes.Public | TypeAttributes.Class
            );
            if (!typeTable.TryAdd(c.Name, c.TypeBuilder))
            {
                throw new RedefinedException(content, c.Name, c.Line, c.Column);
            }
        }

        private SymbolTable<TypeNode> DefineMethod(ClassNode c, MethodNode method)
        {
            SymbolTable<TypeNode> localSymbolTable = new();
            var type = GetTypeFromTypeNode(
                method.ReturnType,
                method.ReturnType.Line,
                method.ReturnType.Column
            );
            method.ReturnType = CreateTypeNodeFromType(type, method.ReturnType.Line, method.ReturnType.Column);
            List<Type> paramTypes = [];
            foreach (var parameter in method.Parameters)
            {
                var paramType = GetTypeFromTypeNode(parameter.Type, parameter.Type.Line, parameter.Type.Column);
                parameter.Type = CreateTypeNodeFromType(paramType, parameter.Type.Line, parameter.Type.Column);
                if (!localSymbolTable.TryAdd(parameter.Name, parameter.Type))
                {
                    throw new RedefinedException(content, parameter.Name, parameter.Line, parameter.Column);
                }
                paramTypes.Add(paramType);
            }
            method.MethodBuilder = c.TypeBuilder!.DefineMethod(
                method.Name,
                method.Attributes,
                GetTypeFromTypeNode(method.ReturnType, method.ReturnType.Line, method.ReturnType.Column),
                [.. paramTypes]
            );
            for (int i = 0; i < method.Parameters.Length; i++)
            {
                int index = method.MethodBuilder.IsStatic ? i : i + 1;
                ParameterBuilder p = method.MethodBuilder.DefineParameter(i + 1, ParameterAttributes.None, method.Parameters[i].Name);
            }
            string methodFullName = string.Join('.', [c.Name, method.Name]);
            if (!moduleMethods.TryAdd((methodFullName, [.. paramTypes]), method.MethodBuilder))
            {
                throw new RedefinedException(content, methodFullName, method.Line, method.Column);
            }
            return localSymbolTable;
        }

        private void AnalyzeUsingNamespaceStatement(UsingNamespaceStatement usingNamespaceStatement)
        {
            Type? type = module.CoreAssembly!.GetType(string.Join(".", usingNamespaceStatement.Namespace));
            if (type != null)
            {
                throw new InvalidUsingStatement(content, string.Join("::", usingNamespaceStatement.Namespace), usingNamespaceStatement.Line, usingNamespaceStatement.Column);
            }
            if (!module.AvailableNamespaces.Contains(string.Join(".", usingNamespaceStatement.Namespace)))
            {
                throw new UnresolvedIdentifierException(
                    content, 
                    string.Join("::", usingNamespaceStatement.Namespace), 
                    usingNamespaceStatement.Line, 
                    usingNamespaceStatement.Column
                );
            }
            usingNamespaceStatement.IsResolved = true;
        }

        private void AnalyzeClass(ClassNode c, SymbolTable<TypeNode>[] localSymbolTableList)
        {
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(c, c.Methods[i], localSymbolTableList[i]);
                VariableNode[] parameters = c.Methods[i].MethodBuilder!.IsStatic ? c.Methods[i].Parameters :
                    [ new VariableNode {
                        Name = "này",
                        Type = StaticAnalyzer.CreateTypeNodeFromType(c.TypeBuilder!, c.Methods[i].ReturnType.Line, c.Methods[i].ReturnType.Column),
                        Column = c.Methods[i].ReturnType.Line,
                        Line = c.Methods[i].ReturnType.Column
                    },..c.Methods[i].Parameters];
                c.Methods[i].Parameters = parameters;
            }
        }

        private void AnalyzeMethod(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable)
        {
            foreach (var statement in method.Statements)
            {
                AnalyzeStatement(c, method, localSymbolTable, statement);
            }
        }

        private void AnalyzeStatement(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, StatementNode statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(c, method, localSymbolTable,  variableDeclarationStatement); break;
                case ExpressionStatement expressionStatement: AnalyzeExpressionStatement(c, method, localSymbolTable, ref expressionStatement); break;
                case AssignmentStatement assignmentStatement: AnalyzeAssignmentStatement(c, method, localSymbolTable, assignmentStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeAssignmentStatement(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, AssignmentStatement assignmentStatement)
        {
            var dest = assignmentStatement.Destination;
            var destType = AnalyzeExpression(c, method, localSymbolTable, ref dest);
            assignmentStatement.Destination = dest;
            var src = assignmentStatement.Source;
            var srcType = AnalyzeExpression(c, method, localSymbolTable, ref src);
            assignmentStatement.Source = src;
            switch(dest)
            {
                case VariableExpression variableExpression:
                    variableExpression.IsLoading = false;
                    break;
                default:
                    throw new IllegalAssignmentException(content, dest.Line, dest.Column);
            }
            AnalyzeTypes(destType, srcType);
        }

        private void AnalyzeExpressionStatement(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref ExpressionStatement expressionStatement)
        {
            var expression = expressionStatement.Expression;
            AnalyzeExpression(c, method, localSymbolTable, ref expression);
            expressionStatement.Expression = expression;
        }

        private void AnalyzeVariableDeclarationStatement(ClassNode c, MethodNode method, SymbolTable<Types.TypeNode> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
        {
            if (localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
            var type = GetTypeFromTypeNode(
                variableDeclarationStatement.Variable.Type,
                variableDeclarationStatement.Variable.Type.Line,
                variableDeclarationStatement.Variable.Type.Column
            );
            variableDeclarationStatement.Variable.Type = CreateTypeNodeFromType(
                type, 
                variableDeclarationStatement.Variable.Type.Line, 
                variableDeclarationStatement.Variable.Type.Column
            );
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(c, method, localSymbolTable, ref expr);
                variableDeclarationStatement.InitialValue = expr;
                AnalyzeTypes(variableDeclarationStatement.Variable.Type, expr.ReturnType!);
            }
            if (!localSymbolTable.TryAdd(variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Type))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
        }

        private void AnalyzeTypes(TypeNode leftNode, TypeNode rightNode)
        {
            Type left = GetTypeFromTypeNode(leftNode, leftNode.Line, leftNode.Column);
            Type right = GetTypeFromTypeNode(rightNode, rightNode.Line, rightNode.Column);
            var sameName = right.FullName == left.FullName;
            var isSubClass = right.IsSubclassOf(left);
            var isAssignable = right.IsAssignableTo(left);
            if (!sameName && !isSubClass && !isAssignable)
            {
                throw new TypeDiscrepancyException(content, leftNode, rightNode, leftNode.Line, leftNode.Column);
            }
        }

        private TypeNode AnalyzeExpression(ClassNode c, MethodNode method, SymbolTable<Types.TypeNode> localSymbolTable, ref ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression variableExpression: return AnalyzeVariableExpression(localSymbolTable, variableExpression);
                case NewArrayExpression newArrayExpression: return AnalyzeNewArrayExpression(c, method, localSymbolTable, ref newArrayExpression);
                case StandardMethodCallExpression standardMethodCallExpression: return AnalyzeStandardMethodCallExpression(c, method, localSymbolTable, ref standardMethodCallExpression);
                case StaticMethodCallExpression staticMethodCallExpression: return AnalyzeStaticMethodCallExpression(c, method, localSymbolTable, staticMethodCallExpression);
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression: 
                    var type = AnalyzeAmbiguousMethodCallExpression(c, method, localSymbolTable, ref ambiguousMethodCallExpression);
                    expression = ambiguousMethodCallExpression;
                    return type;
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(c, method, localSymbolTable,  binaryExpression);
                default: throw new NotImplementedException();
            }
        }

        private TypeNode AnalyzeNewArrayExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref NewArrayExpression newArrayExpression)
        {
            int? size = null;
            Type type = GetTypeFromTypeNode(newArrayExpression.ReturnType!, newArrayExpression.ReturnType!.Line, newArrayExpression.ReturnType.Column);
            newArrayExpression.ReturnType = CreateTypeNodeFromType(type, newArrayExpression.ReturnType!.Line, newArrayExpression.ReturnType.Column);

            // Analyze size expression if not null
            if (newArrayExpression.Size != null)
            {
                ExpressionNode sizeNode = newArrayExpression.Size;
                var sizeType = AnalyzeExpression(c, method, localSymbolTable, ref sizeNode);
                newArrayExpression.Size = sizeNode;
                if (sizeType.CLRFullName != PrimitiveType.Int32 && sizeType.CLRFullName != PrimitiveType.Int64)
                {
                    throw new InvalidArraySizeTypeException(content, sizeType.FullName, sizeNode.Line, sizeNode.Column);
                }
                size = GetValueIfLiteral(sizeNode);
                if (size != null && size < 1)
                {
                    throw new NegativeArraySizeException(content, size.Value, sizeNode.Line, sizeNode.Line);
                }
            }
            // Analyze elements
            for (int i = 0; i < newArrayExpression.Elements.Length; i++)
            {
                TypeNode expressionTypeNode = AnalyzeExpression(c, method, localSymbolTable, ref newArrayExpression.Elements[i]);
                Type expressionType = GetTypeFromTypeNode(expressionTypeNode, expressionTypeNode.Line, expressionTypeNode.Column);
                Type objectType = module.CoreAssembly!.GetType("System.Object")!;
                TypeNode elementTypeNode = ((ArrayType)newArrayExpression.ReturnType).ElementType;
                Type elementType = GetTypeFromTypeNode(elementTypeNode, elementTypeNode.Line, elementTypeNode.Column);
                if (expressionType.IsValueType && elementType == objectType)
                {
                    newArrayExpression.Elements[i] = new BoxExpression
                    {
                        ReturnType = newArrayExpression.Elements[i].ReturnType,
                        Expression = newArrayExpression.Elements[i],
                        Line = newArrayExpression.Elements[i].Line,
                        Column = newArrayExpression.Elements[i].Column
                    };
                }
                else
                {
                    AnalyzeTypes(elementTypeNode, expressionTypeNode);
                }
            }
            // Check if size isn't specified but no initialization
            if (newArrayExpression.Size == null && newArrayExpression.Elements.Length == 0)
            {
                throw new NoSizeArrayWithoutInitializationException(content, newArrayExpression.ReturnType!.Line, newArrayExpression.ReturnType!.Column);
            }
            // Check if size is specified but number of elements is unequal
            if (size != null && size != newArrayExpression.Elements.Length && newArrayExpression.Elements.Length != 0)
            {
                throw new ArraySizeDiscrepancyException(content, size.Value, newArrayExpression.ReturnType!.Line, newArrayExpression.ReturnType.Column, newArrayExpression.Elements.Length, newArrayExpression.Elements[0].Line, newArrayExpression.Elements[0].Column);
            }
            // Ignore size is specified but no initialization but 

            // Ignore if size expression has a variable expression or call expression (size == null && newArrayExpression.Size != null)

            // If size isn't specified but there is initialization (checked from before), set the size for assembler later
            newArrayExpression.Size ??= new LiteralExpression { 
                    ReturnType = CreateTypeNodeFromType(
                        module.CoreAssembly!.GetType(PrimitiveType.Int32)!,
                        newArrayExpression.ReturnType!.Line,
                        newArrayExpression.ReturnType!.Column
                    ),
                    Line = newArrayExpression.ReturnType!.Line, 
                    Column = newArrayExpression.ReturnType!.Column, 
                    Value = newArrayExpression.Elements.Length.ToString() 
                };
            return newArrayExpression.ReturnType!;
        }

        private static int? GetValueIfLiteral(ExpressionNode expression)
        {
            switch(expression)
            {
                case BinaryExpression binaryExpression:
                    var leftValue = GetValueIfLiteral(binaryExpression.Left);
                    var rightValue = GetValueIfLiteral(binaryExpression.Right);
                    if (leftValue != null && rightValue != null)
                    {
                        return binaryExpression.Operator switch
                        {
                            "+" => leftValue + rightValue,
                            "-" => leftValue - rightValue,
                            "*" => leftValue * rightValue,
                            "/" => leftValue / rightValue,
                            _ => throw new NotImplementedException(),
                        };
                    }
                    return null;
                case LiteralExpression literalExpression:
                    return int.Parse(literalExpression.Value);
                default:
                    return null;
            }
        }

        private void AnalyzeArguments<T>(ref T methodCallExpression) where T : AmbiguousMethodCallExpression
        {
            var parameters = methodCallExpression.Info!.GetParameters();
            var objectType = module.CoreAssembly!.GetType("System.Object");
            for (int i = 0; i < parameters.Length; i++)
            {
                if (
                    parameters[i].ParameterType == objectType &&
                    GetTypeFromTypeNode(
                        methodCallExpression.Arguments[i].ReturnType!,
                        methodCallExpression.Arguments[i].Line,
                        methodCallExpression.Arguments[i].Column)
                    .IsValueType
                )
                {
                    methodCallExpression.Arguments[i] = new BoxExpression
                    {
                        ReturnType = methodCallExpression.Arguments[i].ReturnType,
                        Expression = methodCallExpression.Arguments[i],
                        Line = methodCallExpression.Arguments[i].Line,
                        Column = methodCallExpression.Arguments[i].Column
                    };
                }
            }
        }

        private TypeNode AnalyzeStandardMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref StandardMethodCallExpression standardMethodCallExpression)
        {
            var objec = standardMethodCallExpression.Object;
            var typeNode = AnalyzeExpression(c, method, localSymbolTable, ref objec);
            standardMethodCallExpression.Object = objec;
            if (
                method.MethodBuilder!.IsStatic &&
                string.Join(".", [c.Name, standardMethodCallExpression.Name]) ==
                string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.Name])
            )
            {
                throw new StaticIllegalAccessmentException(
                    content,
                    string.Join("::", [c.Name, standardMethodCallExpression.Name]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            var methodFullName = string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.Name]);
            AnalyzeMethodCallExpression(c, method, localSymbolTable, ref standardMethodCallExpression, methodFullName, GetTypeFromTypeNode(typeNode, typeNode.Line, typeNode.Column));
            standardMethodCallExpression.ReturnType = CreateTypeNodeFromType(
                standardMethodCallExpression.Info!.ReturnType,
                standardMethodCallExpression.Line,
                standardMethodCallExpression.Column
            );
            return standardMethodCallExpression.ReturnType;
        }

        private TypeNode AnalyzeStaticMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            staticMethodCallExpression.Type.CLRType = GetTypeFromTypeNode(
                staticMethodCallExpression.Type, 
                staticMethodCallExpression.Line, 
                staticMethodCallExpression.Column
            );
            var methodFullName = string.Join(".", [staticMethodCallExpression.Type.CLRFullName, staticMethodCallExpression.Name]);
            AnalyzeMethodCallExpression(c, method, localSymbolTable, ref staticMethodCallExpression, methodFullName, staticMethodCallExpression.Type.CLRType);
            staticMethodCallExpression.ReturnType = CreateTypeNodeFromType(
                staticMethodCallExpression.Info!.ReturnType, 
                staticMethodCallExpression.Line, 
                staticMethodCallExpression.Column
            );
            return staticMethodCallExpression.ReturnType;
        }

        private TypeNode AnalyzeAmbiguousMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.Name]);
            AnalyzeMethodCallExpression(c, method, localSymbolTable, ref ambiguousMethodCallExpression, methodFullName);
            if (ambiguousMethodCallExpression.Info!.IsStatic)
            {
                var typeNode = CreateTypeNodeFromType(
                    ambiguousMethodCallExpression.Info.DeclaringType!,
                    ambiguousMethodCallExpression.Line,
                    ambiguousMethodCallExpression.Column
                );
                ambiguousMethodCallExpression = typeNode switch
                {
                    NamedType namedType => new StaticMethodCallExpression
                    {
                        Arguments = ambiguousMethodCallExpression.Arguments,
                        Type = namedType,
                        Name = ambiguousMethodCallExpression.Name,
                        ReturnType = CreateTypeNodeFromType(
                                            ambiguousMethodCallExpression.Info.ReturnType!,
                                            ambiguousMethodCallExpression.Line,
                                            ambiguousMethodCallExpression.Column
                                            ),
                        Info = ambiguousMethodCallExpression.Info,
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    _ => throw new("Internal error"),
                };
            }
            else
            {
                if (method.MethodBuilder!.IsStatic)
                {
                    throw new StaticIllegalAccessmentException(
                        content, 
                        string.Join("::", [c.Name, ambiguousMethodCallExpression.Name]), 
                        ambiguousMethodCallExpression.Line, 
                        ambiguousMethodCallExpression.Column
                    );
                }
                ambiguousMethodCallExpression = new StandardMethodCallExpression
                {
                    Arguments = ambiguousMethodCallExpression.Arguments,
                    Object = new ThisExpression {
                        ReturnType = new NamedType {
                            Namespaces = [c.Name],
                            Name = "này",
                            CLRType = typeTable.GetValueOrDefault(c.Name),
                            Line = ambiguousMethodCallExpression.Line,
                            Column = ambiguousMethodCallExpression.Column
                        },
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    Name = ambiguousMethodCallExpression.Name,
                    ReturnType = CreateTypeNodeFromType(
                        ambiguousMethodCallExpression.Info.ReturnType, 
                        ambiguousMethodCallExpression.Line, 
                        ambiguousMethodCallExpression.Column
                    ),
                    Info = ambiguousMethodCallExpression.Info,
                    Line = ambiguousMethodCallExpression.Line,
                    Column = ambiguousMethodCallExpression.Column
                };
            }
            return ambiguousMethodCallExpression.ReturnType!;
        }

        private void AnalyzeMethodCallExpression<T>(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref T methodCallExpression, string methodFullName, Type? type = null) where T : AmbiguousMethodCallExpression
        {
            // Analyze the argument expressions
            List<Type> argumentTypes = [];
            for (int i = 0; i < methodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, method, localSymbolTable, ref methodCallExpression.Arguments[i]);
                argumentTypes.Add(
                    GetTypeFromTypeNode(
                        typ,
                        methodCallExpression.Arguments[i].Line,
                        methodCallExpression.Arguments[i].Column
                    )
                );
            }
            // Get method info
            if (moduleMethods.TryGetValue(
                (methodFullName,
               [.. argumentTypes]),
                out var methodInfo
                )
            )
            {
                methodCallExpression.Info = methodInfo;
            }
            else
            {
                methodCallExpression.Info = type?.GetMethod(
                    methodCallExpression.Name,
                    [.. argumentTypes]
                ) ?? throw new UnresolvedIdentifierException(
                    content,
                    methodCallExpression.Name,
                    methodCallExpression.Line,
                    methodCallExpression.Column
                );
            }
            // Analyze the argument again in case of needing boxing
            AnalyzeArguments(ref methodCallExpression);
        }

        private TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                string[] namespaceAndName = PrimitiveType.String.Split(".");
                literalExpression.ReturnType = new NamedType { 
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.String), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column
                };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                string[] namespaceAndName = PrimitiveType.Int64.Split(".");
                literalExpression.ReturnType = new NamedType { 
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int64), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column 
                };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int32.TryParse(literalExpression.Value, out _))
            {
                string[] namespaceAndName = PrimitiveType.Int32.Split(".");
                literalExpression.ReturnType = new NamedType {
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int32), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column 
                };
            }
            else
            {
                throw new NotImplementedException();
            }
            return literalExpression.ReturnType;
        }

        private Types.TypeNode AnalyzeVariableExpression(SymbolTable<Types.TypeNode> localSymbolTable,  VariableExpression variableExpression)
        {
            if (localSymbolTable.TryGetValue(variableExpression.Name, out var type))
            {
                variableExpression.ReturnType = type;
                return type!;
            }
            else
            {
                throw new UnresolvedIdentifierException(content, variableExpression.Name, variableExpression.Line, variableExpression.Column);
            }
        }

        private Types.TypeNode AnalyzeBinaryExpression(ClassNode c, MethodNode method, SymbolTable<Types.TypeNode> localSymbolTable, BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(c, method, localSymbolTable,  ref left);
            var rightType = AnalyzeExpression(c, method, localSymbolTable,  ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = leftType.CLRFullName;
            var rightTypeFullName = rightType.CLRFullName;
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnType = rightType;
            return rightType;
        }

        /* Helper */

        private Type GetTypeFromTypeNode(TypeNode typeNode, int line, int column)
        {
            if (typeTable.TryGetValue(typeNode.CLRFullName, out var t))
            {
                return t;
            }
            string typeNamespace = string.Join(".", typeNode.Namespaces);
            string typeName = typeNode.Name;
            if (string.IsNullOrEmpty(typeName))
            {
                throw new ArgumentException("Internal error");
            }
            switch (typeNode)
            {
                case ArrayType arrayTypeNode:
                    var arrayType = GetTypeFromTypeNode(arrayTypeNode.ElementType, line, column).MakeArrayType(1);
                    if (!typeTable.TryAdd(typeNode.CLRFullName, arrayType))
                    {
                        throw new("Internal error");
                    }
                    return arrayType;
                case NamedType namedType:
                    Type? type = null;
                    var namespaces = module.UsingNamespaces
                        .Select(n => string.Join(".", n.Namespace))
                        .Where(e => e.EndsWith(typeNamespace))
                        .ToArray();
                    if (namespaces.Length == 0)
                    {
                        namespaces = [typeNamespace];
                    }
                    foreach (var n in namespaces)
                    {
                        var tempType = module.CoreAssembly!.GetType(string.Join(".", [n, typeName]));
                        if (tempType != null && type != null && tempType.FullName != type.FullName)
                        {
                            string tn = typeNode.FullName;
                            string ftn = string.Join("::", type.FullName!.Split("."));
                            string stn = string.Join("::", tempType.FullName!.Split("."));
                            throw new AmbiguousTypeUsage(content, tn, ftn, stn, line, column);
                        }
                        type = tempType;
                    }
                    if (type == null)
                    {
                        throw new UnresolvedTypeException(content, typeNode.FullName, line, column);
                    }
                    if (!typeTable.TryAdd(typeNode.CLRFullName, type))
                    {
                        throw new("Internal error");
                    }
                    return type;
                default:
                    throw new NotImplementedException();
            }
        }

        static public TypeNode CreateTypeNodeFromType(Type type, int line, int column)
        {
            var namespaces = type.Namespace?.Split(".") ?? [];
            if (type.IsArray)
            {
                Type elementType = type.GetElementType()!;
                return new ArrayType
                {
                    ElementType = CreateTypeNodeFromType(elementType, line, column),
                    Namespaces = namespaces,
                    Name = type.Name,
                    Column = column,
                    Line = line,
                };
            }
            return new NamedType { 
                CLRType = type,
                Namespaces = namespaces,
                Name = type.Name,
                Column = column,
                Line = line
            };
        }

        private sealed class MethodSignatureComparer : IEqualityComparer<(string MethodName, Type[] ArgumentTypes)>
        {
            public bool Equals((string MethodName, Type[] ArgumentTypes) x, (string MethodName, Type[] ArgumentTypes) y)
            {
                if (!string.Equals(x.MethodName, y.MethodName, StringComparison.Ordinal)) return false;
                var xa = x.ArgumentTypes;
                var ya = y.ArgumentTypes;
                if (ReferenceEquals(xa, ya)) return true;
                if (xa is null || ya is null) return false;
                return xa.SequenceEqual(ya);
            }

            public int GetHashCode((string MethodName, Type[] ArgumentTypes) obj)
            {
                var hash = new HashCode();
                hash.Add(obj.MethodName, StringComparer.Ordinal);
                if (obj.ArgumentTypes != null)
                {
                    foreach (var t in obj.ArgumentTypes)
                    {
                        hash.Add(t);
                    }
                }
                return hash.ToHashCode();
            }
        }
    }
}
