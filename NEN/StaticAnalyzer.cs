using NEN.Exceptions;
using NEN.Types;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace NEN
{
    public partial class StaticAnalyzer(string[] contentLines, Types.Module module, string assemblyName, string[] assemblyPaths)
    {
        private readonly string[] content = contentLines;
        private readonly Dictionary<string, Type> typeTable = [];
        private readonly Dictionary<(string, Type[]), MethodInfo> moduleMethods = new(new MethodSignatureComparer());
        private readonly Dictionary<(string, Type[]), ConstructorInfo> moduleConstructors = new(new MethodSignatureComparer());
        private readonly Dictionary<string, FieldInfo> moduleFields = [];
        private readonly Types.Module module = module;
        private Types.MethodBase? currentMethod = null;

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
            // Define every field in the classes
            foreach(var c in module.Classes)
            {
                foreach (var field in c.Fields)
                {
                    if (c.Fields.Where(f => f.Variable.Name == field.Variable.Name).ToArray().Length > 1)
                    {
                        throw new RedefinedException(content, field.Variable.Name, field.Variable.Line, field.Variable.Column);
                    }
                    field.FieldInfo = c.TypeBuilder!.DefineField(
                        field.Variable.Name, 
                        GetTypeFromTypeNode(
                            field.Variable.TypeNode), 
                        field.FieldAttributes
                        );
                    if (!moduleFields.TryAdd(string.Join(".", [c.Name, field.Variable.Name]), field.FieldInfo)) 
                    {
                        throw new("Internal error");
                    }
                }
                // Decide if type needs to generate a default constructor
                var fieldsWithInitialization = c.Fields.Where(f => f.InitialValue != null).ToArray();
                if (c.Constructors != null && fieldsWithInitialization.Length > 0)
                    throw new FieldInitializationOutsideDefaultConstructorException(
                        contentLines,
                        fieldsWithInitialization.First().Line,
                        fieldsWithInitialization.First().Column
                    );
                if (c.Constructors == null && fieldsWithInitialization.Length > 0) GenerateDefaultConstructor(c);
            }
            // Define every module in the classes
            List<Dictionary<string, (TypeNode, LocalBuilder)>[]> lsLsSt = [];
            foreach (var c in module.Classes)
            {
                List<Dictionary<string, (TypeNode, LocalBuilder)>> lsSt = [];
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
                        (x, y) => 
                        x.Concat(y).Distinct().ToList()!
                        )
                    ];
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

        private Dictionary<string, (TypeNode, LocalBuilder)> DefineMethod(ClassNode c, MethodNode method)
        {
            Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable = new();
            var type = GetTypeFromTypeNode(
                method.ReturnTypeNode);
            method.ReturnTypeNode = CreateTypeNodeFromType(type, method.ReturnTypeNode.Line, method.ReturnTypeNode.Column);
            List<Type> paramTypes = [];
            foreach (var parameter in method.Parameters)
            {
                var paramType = GetTypeFromTypeNode(parameter.TypeNode);
                parameter.TypeNode = CreateTypeNodeFromType(paramType, parameter.TypeNode.Line, parameter.TypeNode.Column);
                if (method.Parameters.Where(p => p.Name == parameter.Name).ToArray().Length > 1)
                {
                    throw new RedefinedException(content, parameter.Name, parameter.Line, parameter.Column);
                }
                paramTypes.Add(paramType);
            }
            method.MethodBuilder = c.TypeBuilder!.DefineMethod(
                method.MethodName,
                method.MethodAttributes,
                GetTypeFromTypeNode(method.ReturnTypeNode),
                [.. paramTypes]
            );
            for (int i = 0; i < method.Parameters.Length; i++)
            {
                int index = method.MethodBuilder.IsStatic ? i : i + 1;
                ParameterBuilder p = method.MethodBuilder.DefineParameter(index, ParameterAttributes.None, method.Parameters[i].Name);
            }
            string methodFullName = string.Join('.', [c.Name, method.MethodName]);
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

        private void AnalyzeClass(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)>[] localSymbolTableList)
        {
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(c, c.Methods[i], localSymbolTableList[i]);
                VariableNode[] parameters = c.Methods[i].MethodBuilder!.IsStatic ? c.Methods[i].Parameters :
                    [ new VariableNode {
                        Name = "này",
                        TypeNode = CreateTypeNodeFromType(c.TypeBuilder!, c.Methods[i].ReturnTypeNode.Line, c.Methods[i].ReturnTypeNode.Column),
                        Column = c.Methods[i].ReturnTypeNode.Line,
                        Line = c.Methods[i].ReturnTypeNode.Column
                    },..c.Methods[i].Parameters];
                c.Methods[i].Parameters = parameters;
            }
        }

        private void AnalyzeMethod(ClassNode c, MethodNode method, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable)
        {
            currentMethod = method;
            foreach (var statement in method.Statements)
            {
                AnalyzeStatement(c, localSymbolTable, statement);
            }
            currentMethod = null;
        }

        private void AnalyzeStatement(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StatementNode statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(c, localSymbolTable,  variableDeclarationStatement); break;
                case ExpressionStatement expressionStatement: AnalyzeExpressionStatement(c, localSymbolTable, ref expressionStatement); break;
                case AssignmentStatement assignmentStatement: AnalyzeAssignmentStatement(c, localSymbolTable, assignmentStatement); break;
                case ReturnStatement returnStatement: AnalyzeReturnStatement(c, localSymbolTable, returnStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeReturnStatement(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ReturnStatement returnStatement)
        {
            if (currentMethod == null)
            {
                throw new("Internal error");
            }
            TypeNode type = CreateTypeNodeFromType(
                module.CoreAssembly!.GetType("System.Void")!, 
                returnStatement.Line, 
                returnStatement.Column
            );
            if (returnStatement.Expression != null)
            {
                var expression = returnStatement.Expression;
                type = AnalyzeExpression(c, localSymbolTable, ref expression);
                returnStatement.Expression = expression;
            }
            AnalyzeTypes(currentMethod.ReturnTypeNode, type);
        }

        private void AnalyzeAssignmentStatement(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, AssignmentStatement assignmentStatement)
        {
            var dest = assignmentStatement.Destination;
            var destType = AnalyzeExpression(c, localSymbolTable, ref dest);
            assignmentStatement.Destination = dest;
            var src = assignmentStatement.Source;
            var srcType = AnalyzeExpression(c, localSymbolTable, ref src);
            assignmentStatement.Source = src;
            switch(dest)
            {
                case VariableExpression variableExpression:
                    variableExpression.IsLoading = false;
                    break;
                case ArrayIndexingExpression arrayIndexingExpression:
                    arrayIndexingExpression.IsLoading = false;
                    break;
                case FieldAccessmentExpression fieldAccessmentExpression:
                    fieldAccessmentExpression.IsLoading = false;
                    break;
                default:
                    throw new IllegalAssignmentException(content, dest.Line, dest.Column);
            }
            AnalyzeTypes(destType, srcType);
        }

        private void AnalyzeExpressionStatement(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionStatement expressionStatement)
        {
            var expression = expressionStatement.Expression;
            AnalyzeExpression(c, localSymbolTable, ref expression);
            expressionStatement.Expression = expression;
        }

        private void AnalyzeVariableDeclarationStatement(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
        {
            if (
                localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _) || 
                currentMethod?.Parameters.FirstOrDefault(p => p.Name == variableDeclarationStatement.Variable.Name) != null
            )
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
            var type = GetTypeFromTypeNode(variableDeclarationStatement.Variable.TypeNode);
            variableDeclarationStatement.Variable.TypeNode = CreateTypeNodeFromType(
                type, 
                variableDeclarationStatement.Variable.TypeNode.Line, 
                variableDeclarationStatement.Variable.TypeNode.Column
            );
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(c, localSymbolTable, ref expr);
                variableDeclarationStatement.InitialValue = expr;
                AnalyzeTypes(variableDeclarationStatement.Variable.TypeNode, expr.ReturnTypeNode!);
            }
            var methodBase = currentMethod!.GetMethodInfo();
            switch(methodBase)
            {
                case MethodBuilder methodBuilder:
                    var localBuilder = methodBuilder.GetILGenerator().DeclareLocal(type);
                    localBuilder.SetLocalSymInfo(variableDeclarationStatement.Variable.Name);
                    variableDeclarationStatement.LocalBuilder = localBuilder;
                    break;
                default:
                    throw new("Internal error");
            }
            if (!localSymbolTable.TryAdd(
                variableDeclarationStatement.Variable.Name, 
                (variableDeclarationStatement.Variable.TypeNode, variableDeclarationStatement.LocalBuilder)
                )
            )
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
        }

        private bool AnalyzeTypes(TypeNode leftNode, TypeNode rightNode)
        {
            Type left = GetTypeFromTypeNode(leftNode);
            Type right = GetTypeFromTypeNode(rightNode);
            var sameName = right.FullName == left.FullName;
            var isSubClass = right.IsSubclassOf(left);
            var isAssignable = right.IsAssignableTo(left);
            if (!sameName && !isSubClass && !isAssignable)
            {
                throw new TypeDiscrepancyException(content, leftNode, rightNode, leftNode.Line, leftNode.Column);
            }
            return true;
        }

        private TypeNode AnalyzeExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression _: return AnalyzeVariableExpression(c, localSymbolTable, ref expression);
                case NewArrayExpression newArrayExpression: return AnalyzeNewArrayExpression(c, localSymbolTable, ref newArrayExpression);
                case NewObjectExpression newObjectExpression: return AnalyzeNewObjectExpression(c, localSymbolTable, newObjectExpression);
                case StandardMethodCallExpression standardMethodCallExpression: return AnalyzeStandardMethodCallExpression(c, localSymbolTable, ref standardMethodCallExpression);
                case StaticMethodCallExpression staticMethodCallExpression: return AnalyzeStaticMethodCallExpression(c, localSymbolTable, staticMethodCallExpression);
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression: 
                    var type = AnalyzeAmbiguousMethodCallExpression(c, localSymbolTable, ref ambiguousMethodCallExpression);
                    expression = ambiguousMethodCallExpression;
                    return type;
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(c, localSymbolTable,  binaryExpression);
                case ArrayIndexingExpression arrayIndexingExpression: return AnalyzeArrayIndexingExpression(c, localSymbolTable, arrayIndexingExpression);
                case StandardFieldAccessmentExpression standardFieldAccessmentExpression: return AnalyzeStandardFieldAccessmentExpression(c, localSymbolTable, standardFieldAccessmentExpression);
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression: return AnalyzeStaticFieldAccessmentExpression(c, localSymbolTable, staticFieldAccessmentExpression);
                case BoxExpression boxExpression: return AnalyzeBoxExpression(c, localSymbolTable, boxExpression);
                case ThisExpression thisExpression: return AnalyzeThisExpression(thisExpression);
                case DuplicateExpression duplicateExpression: return AnalyzeDuplicateExpression(duplicateExpression);
                default: throw new NotImplementedException();
            }
        }

        private TypeNode AnalyzeDuplicateExpression(DuplicateExpression duplicateExpression)
        {
            var returnType = GetTypeFromTypeNode(duplicateExpression.ReturnTypeNode!);
            duplicateExpression.ReturnTypeNode = CreateTypeNodeFromType(returnType, duplicateExpression.Line, duplicateExpression.Column);
            return duplicateExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeThisExpression(ThisExpression thisExpression)
        {
            var returnType = GetTypeFromTypeNode(thisExpression.ReturnTypeNode!);
            thisExpression.ReturnTypeNode = CreateTypeNodeFromType(returnType, thisExpression.Line, thisExpression.Column);
            return thisExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeBoxExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, BoxExpression boxExpression)
        {
            var expr = boxExpression.Expression;
            var type = AnalyzeExpression(c, localSymbolTable, ref expr);
            boxExpression.Expression = expr;
            boxExpression.ReturnTypeNode = type;
            return type;
        }

        private TypeNode AnalyzeStaticFieldAccessmentExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StaticFieldAccessmentExpression staticFieldAccessmentExpression)
        {
            var fieldType = GetTypeFromTypeNode(staticFieldAccessmentExpression.TypeNode);
            staticFieldAccessmentExpression.TypeNode = CreateTypeNodeFromType(
                fieldType,
                staticFieldAccessmentExpression.Line,
                staticFieldAccessmentExpression.Column
            );
            if (staticFieldAccessmentExpression.FieldInfo != null) { }
            else if (moduleFields.TryGetValue(
                string.Join(
                    ".", 
                [
                        ..staticFieldAccessmentExpression.TypeNode.Namespaces, 
                        staticFieldAccessmentExpression.TypeNode.Name, 
                        staticFieldAccessmentExpression.FieldName
                        ]), 
                out var fieldInfo
                )
            )
            {
                if (!fieldInfo.IsPublic) 
                    throw new InvalidFieldAccessmentException(
                        content,
                        staticFieldAccessmentExpression!.FieldName,
                        staticFieldAccessmentExpression.Line,
                        staticFieldAccessmentExpression.Column
                    );
                staticFieldAccessmentExpression.FieldInfo = fieldInfo;
            }
            else
            {
                staticFieldAccessmentExpression.FieldInfo = fieldType.GetField(staticFieldAccessmentExpression.FieldName, BindingFlags.Public);
                if (staticFieldAccessmentExpression.FieldInfo == null)
                    throw new InvalidFieldAccessmentException(
                        content,
                        staticFieldAccessmentExpression!.FieldName,
                        staticFieldAccessmentExpression.Line,
                        staticFieldAccessmentExpression.Column
                    );
            }
            staticFieldAccessmentExpression.ReturnTypeNode = CreateTypeNodeFromType(
                staticFieldAccessmentExpression.FieldInfo!.FieldType,
                staticFieldAccessmentExpression.Line,
                staticFieldAccessmentExpression.Column
            );
            return staticFieldAccessmentExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeStandardFieldAccessmentExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StandardFieldAccessmentExpression standardFieldAccessmentExpression)
        {
            var fieldObject = standardFieldAccessmentExpression.Object;
            var objectTypeNode = AnalyzeExpression(c, localSymbolTable, ref fieldObject);
            standardFieldAccessmentExpression.Object = fieldObject;
            if (standardFieldAccessmentExpression.FieldInfo != null) { }
            else if (moduleFields.TryGetValue(
                string.Join(
                    ".", 
                    [
                        ..objectTypeNode.Namespaces, 
                        objectTypeNode.Name, 
                        standardFieldAccessmentExpression.FieldName
                        ]
                    ), 
                out var fieldInfo
                )
            )
            {
                if (!fieldInfo.IsPublic) 
                    throw new InvalidFieldAccessmentException(
                        content,
                        standardFieldAccessmentExpression!.FieldName,
                        standardFieldAccessmentExpression.Line,
                        standardFieldAccessmentExpression.Column
                    );
                standardFieldAccessmentExpression.FieldInfo = fieldInfo;
            }
            else
            {
                var fieldObjectType = GetTypeFromTypeNode(fieldObject.ReturnTypeNode!);
                if (standardFieldAccessmentExpression.FieldInfo == null)
                    standardFieldAccessmentExpression.FieldInfo = fieldObjectType.GetField(standardFieldAccessmentExpression.FieldName, BindingFlags.Public);
                if (standardFieldAccessmentExpression.FieldInfo == null)
                    throw new InvalidFieldAccessmentException(
                        content,
                        standardFieldAccessmentExpression!.FieldName,
                        standardFieldAccessmentExpression.Line,
                        standardFieldAccessmentExpression.Column
                    );
            }
            standardFieldAccessmentExpression.ReturnTypeNode = CreateTypeNodeFromType(
                standardFieldAccessmentExpression.FieldInfo!.FieldType,
                standardFieldAccessmentExpression.Line,
                standardFieldAccessmentExpression.Column
            );
            return standardFieldAccessmentExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeNewObjectExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, NewObjectExpression newObjectExpression)
        {
            foreach(var statement in newObjectExpression.FieldInitializations)
            {
                AnalyzeAssignmentStatement(c, localSymbolTable, statement);
            }
            var returnType = GetTypeFromTypeNode(newObjectExpression.ReturnTypeNode!);
            if (moduleConstructors.TryGetValue((returnType.FullName!, []), out var constructorInfo))
            {
                newObjectExpression.ConstructorInfo = constructorInfo;
            }
            else
            {
                try
                {
                    newObjectExpression.ConstructorInfo = returnType.GetConstructor([]);
                }
                catch (Exception) { }
            }
            if (newObjectExpression.ConstructorInfo == null) 
                throw new UnresolvedConstructorException(
                    content, 
                    returnType.FullName!, 
                    [], 
                    newObjectExpression.Line, 
                    newObjectExpression.Column
                );
            newObjectExpression.ReturnTypeNode = CreateTypeNodeFromType(
                returnType,
                newObjectExpression.ReturnTypeNode!.Line,
                newObjectExpression.ReturnTypeNode.Column
            );
            return newObjectExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeArrayIndexingExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ArrayIndexingExpression arrayIndexingExpression)
        {
            var arrayNode = arrayIndexingExpression.Array;
            AnalyzeExpression(c, localSymbolTable, ref arrayNode);
            arrayIndexingExpression.Array = arrayNode;
            var arrayType = GetTypeFromTypeNode(arrayNode.ReturnTypeNode!);
            if (!arrayType.IsArray)
            {
                throw new IndexingOnNonArrayException(content, arrayNode.ReturnTypeNode!.FullName, arrayIndexingExpression.Line, arrayIndexingExpression.Column);
            }
            var indexNode = arrayIndexingExpression.Index;
            AnalyzeExpression(c, localSymbolTable, ref indexNode);
            arrayIndexingExpression.Index = indexNode;
            if (indexNode.ReturnTypeNode!.CLRFullName != PrimitiveType.Int32 && indexNode.ReturnTypeNode.CLRFullName != PrimitiveType.Int64)
            {
                throw new InvalidArrayIndexingTypeException(content, indexNode.ReturnTypeNode.FullName, indexNode.Line, indexNode.Column);
            }
            arrayIndexingExpression.ReturnTypeNode = CreateTypeNodeFromType(arrayType.GetElementType()!, arrayIndexingExpression.Line, arrayIndexingExpression.Column);
            return arrayIndexingExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeNewArrayExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref NewArrayExpression newArrayExpression)
        {
            int? size = null;
            Type type = GetTypeFromTypeNode(newArrayExpression.ReturnTypeNode!);
            newArrayExpression.ReturnTypeNode = CreateTypeNodeFromType(type, newArrayExpression.ReturnTypeNode!.Line, newArrayExpression.ReturnTypeNode.Column);

            // Analyze size expression if not null
            if (newArrayExpression.Size != null)
            {
                ExpressionNode sizeNode = newArrayExpression.Size;
                var sizeType = AnalyzeExpression(c, localSymbolTable, ref sizeNode);
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
                TypeNode expressionTypeNode = AnalyzeExpression(c, localSymbolTable, ref newArrayExpression.Elements[i]);
                Type expressionType = GetTypeFromTypeNode(expressionTypeNode);
                Type objectType = module.CoreAssembly!.GetType("System.Object")!;
                TypeNode elementTypeNode = ((ArrayType)newArrayExpression.ReturnTypeNode).ElementTypeNode;
                Type elementType = GetTypeFromTypeNode(elementTypeNode);
                if (expressionType.IsValueType && elementType == objectType)
                {
                    newArrayExpression.Elements[i] = new BoxExpression
                    {
                        ReturnTypeNode = newArrayExpression.Elements[i].ReturnTypeNode,
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
                throw new NoSizeArrayWithoutInitializationException(content, newArrayExpression.ReturnTypeNode!.Line, newArrayExpression.ReturnTypeNode!.Column);
            }
            // Check if size is specified but number of elements is unequal
            if (size != null && size != newArrayExpression.Elements.Length && newArrayExpression.Elements.Length != 0)
            {
                throw new ArraySizeDiscrepancyException(
                    content, 
                    size.Value, 
                    newArrayExpression.ReturnTypeNode!.Line, 
                    newArrayExpression.ReturnTypeNode.Column, 
                    newArrayExpression.Elements.Length, 
                    newArrayExpression.Elements[0].Line, 
                    newArrayExpression.Elements[0].Column
                    );
            }
            // Ignore size is specified but no initialization but 

            // Ignore if size expression has a variable expression or call expression (size == null && newArrayExpression.Size != null)

            // If size isn't specified but there is initialization (checked from before), set the size for assembler later
            newArrayExpression.Size ??= new LiteralExpression { 
                    ReturnTypeNode = CreateTypeNodeFromType(
                        module.CoreAssembly!.GetType(PrimitiveType.Int32)!,
                        newArrayExpression.ReturnTypeNode!.Line,
                        newArrayExpression.ReturnTypeNode!.Column
                    ),
                    Line = newArrayExpression.ReturnTypeNode!.Line, 
                    Column = newArrayExpression.ReturnTypeNode!.Column, 
                    Value = newArrayExpression.Elements.Length.ToString() 
                };
            return newArrayExpression.ReturnTypeNode!;
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
            var parameters = methodCallExpression.MethodInfo!.GetParameters();
            var objectType = module.CoreAssembly!.GetType("System.Object");
            for (int i = 0; i < parameters.Length; i++)
            {
                if (
                    parameters[i].ParameterType == objectType &&
                    GetTypeFromTypeNode(
                        methodCallExpression.Arguments[i].ReturnTypeNode!)
                    .IsValueType
                )
                {
                    methodCallExpression.Arguments[i] = new BoxExpression
                    {
                        ReturnTypeNode = methodCallExpression.Arguments[i].ReturnTypeNode,
                        Expression = methodCallExpression.Arguments[i],
                        Line = methodCallExpression.Arguments[i].Line,
                        Column = methodCallExpression.Arguments[i].Column
                    };
                }
            }
        }

        private TypeNode AnalyzeStandardMethodCallExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref StandardMethodCallExpression standardMethodCallExpression)
        {
            if (currentMethod == null)
            {
                throw new MethodCallFromOutsideException(content, standardMethodCallExpression.Line, standardMethodCallExpression.Column);
            }
            var objec = standardMethodCallExpression.Object;
            var typeNode = AnalyzeExpression(c, localSymbolTable, ref objec);
            var methodInfo = standardMethodCallExpression.MethodInfo;
            standardMethodCallExpression.Object = objec;
            if (
                currentMethod.GetMethodInfo()!.IsStatic &&
                string.Join(".", [c.Name, standardMethodCallExpression.MethodName]) ==
                string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.MethodName])
            )
            {
                throw new StaticIllegalAccessmentException(
                    content,
                    string.Join("::", [c.Name, standardMethodCallExpression.MethodName]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            var methodFullName = string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(
                c, 
                localSymbolTable, 
                ref standardMethodCallExpression, 
                methodFullName, 
                GetTypeFromTypeNode(typeNode)
                );
            standardMethodCallExpression.ReturnTypeNode = CreateTypeNodeFromType(
                GetReturnTypeFromMethodBase(standardMethodCallExpression.MethodInfo!)!,
                standardMethodCallExpression.Line,
                standardMethodCallExpression.Column
            );
            return standardMethodCallExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeStaticMethodCallExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            staticMethodCallExpression.TypeNode.CLRType = GetTypeFromTypeNode(
                staticMethodCallExpression.TypeNode);
            var methodFullName = string.Join(".", [staticMethodCallExpression.TypeNode.CLRFullName, staticMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(c, localSymbolTable, ref staticMethodCallExpression, methodFullName, staticMethodCallExpression.TypeNode.CLRType);
            staticMethodCallExpression.ReturnTypeNode = CreateTypeNodeFromType(
                GetReturnTypeFromMethodBase(staticMethodCallExpression.MethodInfo!)!, 
                staticMethodCallExpression.Line, 
                staticMethodCallExpression.Column
            );
            return staticMethodCallExpression.ReturnTypeNode;
        }

        /// <summary>
        /// Will mutate the expression into a StandardMethodCallExpression or a StaticMethodCallExpression
        /// </summary>
        /// <param name="c"></param>
        /// <param name="localSymbolTable"></param>
        /// <param name="ambiguousMethodCallExpression"></param>
        /// <returns></returns>
        /// <exception cref="MethodCallFromOutsideException"></exception>
        /// <exception cref="StaticIllegalAccessmentException"></exception>
        private TypeNode AnalyzeAmbiguousMethodCallExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            if (currentMethod == null)
            {
                throw new MethodCallFromOutsideException(content, ambiguousMethodCallExpression.Line, ambiguousMethodCallExpression.Column);
            }
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(c, localSymbolTable, ref ambiguousMethodCallExpression, methodFullName);
            if (ambiguousMethodCallExpression.MethodInfo!.IsStatic)
            {
                var typeNode = CreateTypeNodeFromType(
                    ambiguousMethodCallExpression.MethodInfo.DeclaringType!,
                    ambiguousMethodCallExpression.Line,
                    ambiguousMethodCallExpression.Column
                );
                ambiguousMethodCallExpression = typeNode switch
                {
                    NamedType namedType => new StaticMethodCallExpression
                    {
                        Arguments = ambiguousMethodCallExpression.Arguments,
                        TypeNode = namedType,
                        MethodName = ambiguousMethodCallExpression.MethodName,
                        ReturnTypeNode = CreateTypeNodeFromType(
                                            GetReturnTypeFromMethodBase(ambiguousMethodCallExpression.MethodInfo)!,
                                            ambiguousMethodCallExpression.Line,
                                            ambiguousMethodCallExpression.Column
                                            ),
                        MethodInfo = ambiguousMethodCallExpression.MethodInfo,
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    _ => throw new("Internal error"),
                };
            }
            else
            {
                if (currentMethod.GetMethodInfo()!.IsStatic)
                {
                    throw new StaticIllegalAccessmentException(
                        content, 
                        string.Join("::", [c.Name, ambiguousMethodCallExpression.MethodName]), 
                        ambiguousMethodCallExpression.Line, 
                        ambiguousMethodCallExpression.Column
                    );
                }
                ambiguousMethodCallExpression = new StandardMethodCallExpression
                {
                    Arguments = ambiguousMethodCallExpression.Arguments,
                    Object = new ThisExpression {
                        ReturnTypeNode = new NamedType {
                            Namespaces = [c.Name],
                            Name = "này",
                            CLRType = typeTable.GetValueOrDefault(c.Name),
                            Line = ambiguousMethodCallExpression.Line,
                            Column = ambiguousMethodCallExpression.Column
                        },
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    MethodName = ambiguousMethodCallExpression.MethodName,
                    ReturnTypeNode = CreateTypeNodeFromType(
                        GetReturnTypeFromMethodBase(ambiguousMethodCallExpression.MethodInfo)!, 
                        ambiguousMethodCallExpression.Line, 
                        ambiguousMethodCallExpression.Column
                    ),
                    MethodInfo = ambiguousMethodCallExpression.MethodInfo,
                    Line = ambiguousMethodCallExpression.Line,
                    Column = ambiguousMethodCallExpression.Column
                };
            }
            return ambiguousMethodCallExpression.ReturnTypeNode!;
        }

        private void AnalyzeMethodCallExpression<T>(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref T methodCallExpression, string methodFullName, Type? type = null) where T : AmbiguousMethodCallExpression
        {
            // Analyze the argument expressions
            List<Type> argumentTypes = [];
            for (int i = 0; i < methodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, localSymbolTable, ref methodCallExpression.Arguments[i]);
                argumentTypes.Add(
                    GetTypeFromTypeNode(
                        typ)
                );
            }
            if (methodCallExpression.MethodInfo == null)
            {
                // Get method info
                if (moduleMethods.TryGetValue(
                    (methodFullName,
                   [.. argumentTypes]),
                    out var methodInfo
                    )
                )
                {
                    var isFromSameOrParentType = false;
                    if (type != null)
                    {
                        try
                        {
                            isFromSameOrParentType = AnalyzeTypes(
                            CreateTypeNodeFromType(methodInfo.DeclaringType!, methodCallExpression.Line, methodCallExpression.Column),
                        CreateTypeNodeFromType(type!, methodCallExpression.Line, methodCallExpression.Column)
                            );
                        } catch(Exception) { }
                    }
                    else
                    {
                        isFromSameOrParentType = true;
                    }
                    if (!methodInfo.IsPublic && !isFromSameOrParentType
                    ) throw new InvalidMethodCallException(
                        content,
                        methodCallExpression.MethodName,
                        [.. argumentTypes],
                        methodCallExpression.Line,
                        methodCallExpression.Column
                    );
                    methodCallExpression.MethodInfo = methodInfo;
                }
                else
                {
                    methodCallExpression.MethodInfo = type?.GetMethod(
                        methodCallExpression.MethodName,
                        [.. argumentTypes]
                    ) ?? throw new InvalidMethodCallException(
                        content,
                        methodCallExpression.MethodName,
                        [..argumentTypes],
                        methodCallExpression.Line,
                        methodCallExpression.Column
                    );

                    if (!methodCallExpression.MethodInfo.IsPublic) throw new InvalidMethodCallException(
                        content,
                        methodCallExpression.MethodName,
                        [.. argumentTypes],
                        methodCallExpression.Line,
                        methodCallExpression.Column
                    );
                }
            }
            // Analyze the argument again in case of needing boxing
            AnalyzeArguments(ref methodCallExpression);
        }

        private TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                string[] namespaceAndName = PrimitiveType.String.Split(".");
                literalExpression.ReturnTypeNode = new NamedType { 
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
                literalExpression.ReturnTypeNode = new NamedType { 
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
                literalExpression.ReturnTypeNode = new NamedType {
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
            return literalExpression.ReturnTypeNode;
        }

        /// <summary>
        /// Can mutate the expression to a StaticFieldAccessmentExpression or a StandardFieldAccessmentExpression
        /// </summary>
        /// <param name="c"></param>
        /// <param name="localSymbolTable"></param>
        /// <param name="expression"></param>
        /// <returns></returns>
        /// <exception cref="UnresolvedIdentifierException"></exception>
        private Types.TypeNode AnalyzeVariableExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionNode expression)
        {
            var variableExpression = (VariableExpression)expression;
            var variableName = variableExpression.Name;
            var argument = currentMethod?.Parameters.Select((p, i) => (p, i)).FirstOrDefault(e =>  e.p.Name == variableName);
            if (argument?.p != null)
            {
                expression = new ArgumentExpression
                {
                    Name = variableName,
                    Index = argument?.i,
                    ReturnTypeNode = argument?.p.TypeNode,
                    Line = variableExpression.Line,
                    Column = variableExpression.Column,
                    IsLoading = variableExpression.IsLoading
                };
                return expression.ReturnTypeNode!;
            }
            var field = c.Fields.FirstOrDefault(f => f.Variable.Name == variableName);
            if (field != null)
            {
                if (field.FieldAttributes.HasFlag(FieldAttributes.Static))
                {
                    expression = new StaticFieldAccessmentExpression
                    {
                        ReturnTypeNode = field.Variable.TypeNode,
                        TypeNode = new NamedType
                        {
                            Namespaces = [], // TODO: change when namespaces are implemented
                            Name = c.Name,
                            Line = expression.Line,
                            Column = expression.Column
                        },
                        FieldName = field.Variable.Name,
                        FieldInfo = field.FieldInfo,
                        Line = expression.Line,
                        Column = expression.Column
                    };
                    AnalyzeStaticFieldAccessmentExpression(c, localSymbolTable, (StaticFieldAccessmentExpression)expression);
                    return expression.ReturnTypeNode;
                }
                else
                {
                    expression = new StandardFieldAccessmentExpression {
                        ReturnTypeNode = field.Variable.TypeNode,
                        Object = new ThisExpression
                        {
                            ReturnTypeNode = new NamedType
                            {
                                Namespaces = [],
                                Name = c.Name,
                                Line = expression.Line,
                                Column = expression.Column
                            },
                            Line = expression.Line,
                            Column = expression.Column,
                        },
                        FieldName = field.Variable.Name,
                        FieldInfo = field.FieldInfo,
                        Line = expression.Line,
                        Column = expression.Column
                    };
                    AnalyzeStandardFieldAccessmentExpression(c, localSymbolTable, (StandardFieldAccessmentExpression)expression);
                    return expression.ReturnTypeNode;
                }
            }
            if (localSymbolTable.TryGetValue(variableExpression.Name, out var res))
            {
                variableExpression.ReturnTypeNode = res.Item1;
                variableExpression.LocalBuilder = res.Item2;
                return res.Item1!;
            }
            else
            {
                throw new UnresolvedIdentifierException(content, variableExpression.Name, variableExpression.Line, variableExpression.Column);
            }
        }

        private Types.TypeNode AnalyzeBinaryExpression(ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(c, localSymbolTable,  ref left);
            var rightType = AnalyzeExpression(c, localSymbolTable,  ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = leftType.CLRFullName;
            var rightTypeFullName = rightType.CLRFullName;
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnTypeNode = rightType;
            return rightType;
        }

        /* Helper */

        static private Type? GetReturnTypeFromMethodBase(System.Reflection.MethodBase methodBase)
        {
            return methodBase switch
            {
                MethodInfo method => method.ReturnType,
                ConstructorInfo constructor => constructor.DeclaringType,
                _ => throw new NotImplementedException(),
            };
        }

        private Type GetTypeFromTypeNode(TypeNode typeNode)
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
                    var elementType = GetTypeFromTypeNode(arrayTypeNode.ElementTypeNode);
                    var arrayType = arrayTypeNode.Rank == 1
                        ? elementType.MakeArrayType()
                        : elementType.MakeArrayType(arrayTypeNode.Rank);
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
                            throw new AmbiguousTypeUsage(content, tn, ftn, stn, typeNode.Line, typeNode.Column);
                        }
                        type = tempType;
                    }
                    if (type == null)
                    {
                        throw new UnresolvedTypeException(content, typeNode.FullName, typeNode.Line, typeNode.Column);
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
                    ElementTypeNode = CreateTypeNodeFromType(elementType, line, column),
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
