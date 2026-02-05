using NEN.Exceptions;
using NEN.AST;
using System.Reflection;
using System.Reflection.Emit;

namespace NEN
{
    public partial class StaticAnalyzer(string assemblyName, ModulePart[] moduleParts, string[] assemblyPaths)
    {
        private readonly Dictionary<string, Type> typeTable = [];
        private readonly Dictionary<(string, Type[]), MethodInfo> moduleMethods = new(new MethodSignatureComparer());
        private readonly Dictionary<(string, Type[]), ConstructorInfo> moduleConstructors = new(new MethodSignatureComparer());
        private readonly Dictionary<string, FieldInfo> moduleFields = [];
        private readonly AST.Module module = new() { Name = assemblyName, ModuleParts = moduleParts};
        private AST.MethodBase? currentMethod = null;

        public AST.Module Analyze()
        {
            SetupModule();
            // Declaring the types and fields in every module
            foreach (var modulePart in module.ModuleParts)
            {
                foreach (var usingNamespaceStatement in modulePart.UsingNamespaces)
                {
                    AnalyzeUsingNamespaceStatement(modulePart, usingNamespaceStatement);
                }
                // Define every class in the module
                foreach (var c in modulePart.Classes)
                {
                    DefineClass(modulePart, c);
                }
                
                foreach (var c in modulePart.Classes)
                {
                    // Define every field in the classes
                    foreach (var field in c.Fields)
                    {
                        if (c.Fields.Where(f => f.Variable.Name == field.Variable.Name).ToArray().Length > 1)
                        {
                            throw new RedefinedException(modulePart.Source, field.Variable.Name, field.Variable.StartLine, field.Variable.StartColumn, field.Variable.EndLine, field.Variable.EndColumn);
                        }
                        field.FieldInfo = c.TypeBuilder!.DefineField(
                            field.Variable.Name,
                            GetTypeFromTypeNode(modulePart, field.Variable.TypeNode),
                            field.FieldAttributes
                            );
                        if (!moduleFields.TryAdd(string.Join(".", [c.Name, field.Variable.Name]), field.FieldInfo))
                        {
                            throw new("Internal error");
                        }
                    }
                    
                }
            }
            // Declaring every method and constructor in every classes in every module
            foreach (var modulePart in module.ModuleParts)
            {
                foreach (var c in modulePart.Classes)
                {
                    // Decide if type needs to generate a default constructor
                    var fieldsWithInitialization = c.Fields
                        .Where(
                            f => f.InitialValue != null && 
                            !f.FieldAttributes.HasFlag(FieldAttributes.Static))
                        .ToArray();
                    if (c.Constructors != null && fieldsWithInitialization.Length > 0)
                        throw new FieldInitializationOutsideDefaultConstructorException(
                            modulePart.Source,
                            fieldsWithInitialization.First().StartLine,
                            fieldsWithInitialization.First().StartColumn,
                            fieldsWithInitialization.First().EndLine,
                            fieldsWithInitialization.First().EndColumn
                        );
                    if (c.Constructors == null) GenerateDefaultConstructor(modulePart, c);
                    else
                    {
                        foreach (var constructor in c.Constructors)
                        {
                            DefineConstructor(modulePart, c, constructor);
                        }
                    }

                    // Define every method in the classes
                    foreach (var method in c.Methods)
                    {
                        DefineMethod(modulePart, c, method);
                    }
                }
            }
            foreach (var modulePart in module.ModuleParts)
            {
                // Analyze the method bodies in each class
                foreach (var c in modulePart.Classes)
                {
                    AnalyzeClass(modulePart, c);
                }
            }
            return module;
        }

        private void SetupModule()
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

        private void DefineClass(ModulePart modulePart, ClassNode c)
        {
            c.TypeBuilder = module.ModuleBuilder!.DefineType(
                c.Name,
                TypeAttributes.Public | TypeAttributes.Class
            );
            if (!typeTable.TryAdd(c.Name, c.TypeBuilder))
            {
                throw new RedefinedException(modulePart.Source, c.Name, c.StartLine, c.StartColumn, c.EndLine, c.EndColumn);
            }
        }

        private void DefineConstructor(ModulePart modulePart, ClassNode c, ConstructorNode constructor)
        {
            constructor.DeclaringTypeNode.CLRType = c.TypeBuilder;
            List<Type> paramTypes = [];
            foreach (var parameter in constructor.Parameters)
            {
                var paramType = GetTypeFromTypeNode(modulePart, parameter.TypeNode);
                parameter.TypeNode = CreateTypeNodeFromType(
                    paramType,
                    parameter.TypeNode.StartLine,
                    parameter.TypeNode.StartColumn,
                    parameter.TypeNode.EndLine,
                    parameter.TypeNode.EndColumn);
                if (constructor.Parameters.Where(p => p.Name == parameter.Name).ToArray().Length > 1)
                {
                    throw new RedefinedException(
                        modulePart.Source, 
                        parameter.Name, 
                        parameter.StartLine, 
                        parameter.StartColumn, 
                        parameter.EndLine, 
                        parameter.EndColumn
                        );
                }
                paramTypes.Add(paramType);
            }
            constructor.ConstructorBuilder = c.TypeBuilder!.DefineConstructor(
                constructor.MethodAttributes,
                CallingConventions.Standard, // maybe change this later
                [..paramTypes]
            );
            for (int i = 0; i < constructor.Parameters.Length; i++)
            {
                ParameterBuilder p = constructor.ConstructorBuilder.DefineParameter(i + 1, ParameterAttributes.None, constructor.Parameters[i].Name);
            }
            if (!moduleConstructors.TryAdd((c.Name, [..paramTypes]), constructor.ConstructorBuilder))
            {
                throw new RedefinedException(
                    modulePart.Source,
                    $"{c.Name}({string.Join(", ", constructor.Parameters.Select(p => p.TypeNode.FullName))})",
                    constructor.StartLine,
                    constructor.StartColumn,
                    constructor.EndLine,
                    constructor.EndColumn
                );
            }
        }

        private void DefineMethod(ModulePart modulePart, ClassNode c, MethodNode method)
        {
            var type = GetTypeFromTypeNode(modulePart, method.ReturnTypeNode);
            method.ReturnTypeNode = CreateTypeNodeFromType(
                type, 
                method.ReturnTypeNode.StartLine, 
                method.ReturnTypeNode.StartColumn,
                method.ReturnTypeNode.EndLine,
                method.ReturnTypeNode.EndColumn);
            List<Type> paramTypes = [];
            foreach (var parameter in method.Parameters)
            {
                var paramType = GetTypeFromTypeNode(modulePart, parameter.TypeNode);
                parameter.TypeNode = CreateTypeNodeFromType(
                    paramType, 
                    parameter.TypeNode.StartLine, 
                    parameter.TypeNode.StartColumn,
                    parameter.TypeNode.EndLine,
                    parameter.TypeNode.EndColumn);
                if (method.Parameters.Where(p => p.Name == parameter.Name).ToArray().Length > 1)
                {
                    throw new RedefinedException(
                        modulePart.Source, 
                        parameter.Name, 
                        parameter.StartLine, 
                        parameter.StartColumn, 
                        parameter.EndLine, 
                        parameter.EndColumn
                        );
                }
                paramTypes.Add(paramType);
            }
            method.MethodBuilder = c.TypeBuilder!.DefineMethod(
                method.MethodName,
                method.MethodAttributes,
                GetTypeFromTypeNode(modulePart, method.ReturnTypeNode),
                [.. paramTypes]
            );
            for (int i = 0; i < method.Parameters.Length; i++)
            {
                int index = i + 1;
                ParameterBuilder p = method.MethodBuilder.DefineParameter(index, ParameterAttributes.None, method.Parameters[i].Name);
            }
            string methodFullName = string.Join('.', [c.Name, method.MethodName]);
            if (!moduleMethods.TryAdd((methodFullName, [.. paramTypes]), method.MethodBuilder))
            {
                throw new RedefinedException(
                    modulePart.Source, 
                    $"{string.Join("::", [c.Name, method.MethodName])}({string.Join(", ", method.Parameters.Select(p => p.TypeNode.FullName))})", 
                    method.StartLine, 
                    method.StartColumn, 
                    method.EndLine, 
                    method.EndColumn
                );
            }
        }

        private void AnalyzeUsingNamespaceStatement(ModulePart modulePart, UsingNamespaceStatement usingNamespaceStatement)
        {
            Type? type = module.CoreAssembly!.GetType(string.Join(".", usingNamespaceStatement.Namespace));
            if (type != null)
            {
                throw new InvalidUsingStatement(modulePart.Source, string.Join("::", usingNamespaceStatement.Namespace), usingNamespaceStatement.StartLine, usingNamespaceStatement.StartColumn, usingNamespaceStatement.EndLine, usingNamespaceStatement.EndColumn);
            }
            if (!module.AvailableNamespaces.Contains(string.Join(".", usingNamespaceStatement.Namespace)))
            {
                throw new UnresolvedIdentifierException(
                    modulePart.Source, 
                    string.Join("::", usingNamespaceStatement.Namespace), 
                    usingNamespaceStatement.StartLine, 
                    usingNamespaceStatement.StartColumn,
                    usingNamespaceStatement.EndLine,
                    usingNamespaceStatement.EndColumn
                );
            }
            usingNamespaceStatement.IsResolved = true;
        }

        private void AnalyzeClass(ModulePart modulePart, ClassNode c)
        {
            foreach (var init in c.Fields
                .Where(f => f.InitialValue != null && f.FieldAttributes.HasFlag(FieldAttributes.Static)))
            {
                AnalyzeFieldDeclarationStatement(modulePart, c, init);
            }
            foreach (var constructor in c.Constructors!) // should have at least one by the time we're analyzing them
            {
                VariableNode[] parameters =
                    [ new VariableNode {
                        Name = "này",
                        TypeNode = CreateTypeNodeFromType(
                            c.TypeBuilder!,
                            constructor.DeclaringTypeNode.StartLine,
                            constructor.DeclaringTypeNode.StartColumn,
                            constructor.DeclaringTypeNode.EndLine,
                            constructor.DeclaringTypeNode.EndColumn
                            ),
                        StartLine = constructor.DeclaringTypeNode.StartLine,
                        StartColumn = constructor.DeclaringTypeNode.StartColumn,
                        EndLine = constructor.DeclaringTypeNode.EndLine,
                        EndColumn = constructor.DeclaringTypeNode.EndColumn
                    },..constructor.Parameters];
                constructor.Parameters = parameters;
                AnalyzeConstructor(modulePart, c, constructor);
            }
            foreach (var method in c.Methods)
            {
                VariableNode[] parameters = method.MethodBuilder!.IsStatic ? method.Parameters :
                    [ new VariableNode {
                        Name = "này",
                        TypeNode = CreateTypeNodeFromType(
                            c.TypeBuilder!,
                            method.ReturnTypeNode.StartLine,
                            method.ReturnTypeNode.StartColumn,
                            method.ReturnTypeNode.EndLine,
                            method.ReturnTypeNode.EndColumn
                            ),
                        StartLine = method.ReturnTypeNode.StartLine,
                        StartColumn = method.ReturnTypeNode.StartColumn,
                        EndLine = method.ReturnTypeNode.EndLine,
                        EndColumn = method.ReturnTypeNode.EndColumn
                    },..method.Parameters];
                method.Parameters = parameters;
                AnalyzeMethod(modulePart, c, method);
            }
        }

        private void AnalyzeConstructor(ModulePart modulePart, ClassNode c, ConstructorNode constructor)
        {
            Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable = [];
            currentMethod = constructor;
            foreach (var statement in constructor.Statements)
            {
                AnalyzeStatement(modulePart, c, localSymbolTable, statement);
            }
            currentMethod = null;
        }

        private void AnalyzeMethod(ModulePart modulePart, ClassNode c, MethodNode method)
        {
            Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable = [];
            currentMethod = method;
            foreach (var statement in method.Statements)
            {
                AnalyzeStatement(modulePart, c, localSymbolTable, statement);
            }
            currentMethod = null;
        }

        private void AnalyzeStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StatementNode statement, Label? endLabel = null)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(modulePart, c, localSymbolTable,  variableDeclarationStatement); break;
                case ExpressionStatement expressionStatement: AnalyzeExpressionStatement(modulePart, c, localSymbolTable, ref expressionStatement); break;
                case AssignmentStatement assignmentStatement: AnalyzeAssignmentStatement(modulePart, c, localSymbolTable, assignmentStatement); break;
                case ReturnStatement returnStatement: AnalyzeReturnStatement(modulePart, c, localSymbolTable, returnStatement); break;
                case IfStatement ifStatement: AnalyzeIfStatement(modulePart, c, localSymbolTable, ifStatement, endLabel); break;
                case WhileStatement whileStatement: AnalyzeWhileStatement(modulePart, c, localSymbolTable, whileStatement); break;
                case BreakStatement breakStatement: AnalyzeBreakStatement(modulePart, breakStatement, endLabel); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeBreakStatement(ModulePart modulePart, BreakStatement breakStatement, Label? endLabel)
        {
            breakStatement.EndLabel = endLabel ?? throw new BreakOutsideLoopException(modulePart.Source, breakStatement.StartLine, breakStatement.StartColumn, breakStatement.EndLine, breakStatement.EndColumn);
        }

        private void AnalyzeWhileStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, WhileStatement whileStatement)
        {
            var condition = whileStatement.Condition;
            var conditionType = AnalyzeExpression(modulePart, c, localSymbolTable, ref condition);
            whileStatement.Condition = condition;
            if (conditionType!.CLRFullName != PrimitiveType.Boolean)
                throw new InvalidIfConditionTypeException(
                    modulePart.Source,
                    condition.ReturnTypeNode!.FullName,
                    condition.StartLine,
                    condition.StartColumn,
                    condition.EndLine,
                    condition.EndColumn
                );
            switch (currentMethod!.GetMethodInfo())
            {
                case MethodBuilder methodBuilder:
                    whileStatement.EndLabel = methodBuilder.GetILGenerator().DefineLabel();
                    break;
                default:
                    throw new NotImplementedException();
            }
            var newLocalSymbolTable = new Dictionary<string, (TypeNode, LocalBuilder)>(localSymbolTable);
            foreach(var statement in whileStatement.Body)
            {
                AnalyzeStatement(modulePart, c, newLocalSymbolTable, statement, whileStatement.EndLabel);
            }
        }

        private void AnalyzeIfStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, IfStatement ifStatement, Label? endLabel = null)
        {
            var condition = ifStatement.Condition;
            var conditionType = AnalyzeExpression(modulePart, c, localSymbolTable, ref condition);
            ifStatement.Condition = condition;
            if (conditionType!.CLRFullName != PrimitiveType.Boolean)
                throw new InvalidIfConditionTypeException(
                    modulePart.Source,
                    condition.ReturnTypeNode!.FullName,
                    condition.StartLine,
                    condition.StartColumn,
                    condition.EndLine,
                    condition.EndColumn
                );
            var ifLocalSymbolTable = new Dictionary<string, (TypeNode, LocalBuilder)>(localSymbolTable);
            foreach (var statement in ifStatement.IfClause)
            {
                AnalyzeStatement(modulePart, c, ifLocalSymbolTable, statement, endLabel);
            }
            var elseLocalSymbolTable = new Dictionary<string, (TypeNode, LocalBuilder)>(localSymbolTable);
            foreach (var statement in ifStatement.ElseClause)
            {
                AnalyzeStatement(modulePart, c, elseLocalSymbolTable, statement, endLabel);
            }
        }

        private void AnalyzeReturnStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ReturnStatement returnStatement)
        {
            if (currentMethod == null)
            {
                throw new("Internal error");
            }
            TypeNode type = CreateTypeNodeFromType(
                module.CoreAssembly!.GetType("System.Void")!, 
                returnStatement.StartLine, 
                returnStatement.StartColumn,
                returnStatement.EndLine,
                returnStatement.EndColumn
            );
            if (returnStatement.Expression != null)
            {
                var expression = returnStatement.Expression;
                type = AnalyzeExpression(modulePart, c, localSymbolTable, ref expression);
                returnStatement.Expression = expression;
            }
            switch(currentMethod)
            {
                case MethodNode method:
                    AnalyzeTypes(modulePart, method.ReturnTypeNode, type); break;
                case ConstructorNode constructor:
                    AnalyzeTypes(modulePart, constructor.DeclaringTypeNode, type); break;
            }
        }

        private void AnalyzeAssignmentStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, AssignmentStatement assignmentStatement)
        {
            var dest = assignmentStatement.Destination;
            var destType = AnalyzeExpression(modulePart, c, localSymbolTable, ref dest);
            assignmentStatement.Destination = dest;
            var src = assignmentStatement.Source;
            var srcType = AnalyzeExpression(modulePart, c, localSymbolTable, ref src);
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
                    throw new IllegalAssignmentException(modulePart.Source, dest.StartLine, dest.StartColumn, dest.EndLine, dest.EndColumn);
            }
            AnalyzeTypes(modulePart, destType, srcType);
        }

        private void AnalyzeExpressionStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionStatement expressionStatement)
        {
            var expression = expressionStatement.Expression;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref expression);
            expressionStatement.Expression = expression;
        }

        private void AnalyzeFieldDeclarationStatement(ModulePart modulePart, ClassNode c, FieldDeclarationStatement fieldDeclarationStatement)
        {
            var type = GetTypeFromTypeNode(modulePart, fieldDeclarationStatement.Variable.TypeNode);
            fieldDeclarationStatement.Variable.TypeNode = CreateTypeNodeFromType(
                type,
                fieldDeclarationStatement.Variable.TypeNode.StartLine,
                fieldDeclarationStatement.Variable.TypeNode.StartColumn,
                fieldDeclarationStatement.Variable.TypeNode.EndLine,
                fieldDeclarationStatement.Variable.TypeNode.EndColumn
            );
            if (fieldDeclarationStatement.InitialValue != null) {
                var expr = fieldDeclarationStatement.InitialValue;
                AnalyzeExpression(modulePart, c, [], ref expr);
                fieldDeclarationStatement.InitialValue = expr;
                AnalyzeTypes(modulePart, fieldDeclarationStatement.Variable.TypeNode, expr.ReturnTypeNode!);
            }
        }

        private void AnalyzeVariableDeclarationStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
        {
            if (
                localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _) || 
                currentMethod?.Parameters.FirstOrDefault(p => p.Name == variableDeclarationStatement.Variable.Name) != null
            )
            {
                throw new RedefinedException(modulePart.Source, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.StartLine, variableDeclarationStatement.Variable.StartColumn, variableDeclarationStatement.Variable.EndLine, variableDeclarationStatement.Variable.EndColumn);
            }
            var type = GetTypeFromTypeNode(modulePart, variableDeclarationStatement.Variable.TypeNode);
            variableDeclarationStatement.Variable.TypeNode = CreateTypeNodeFromType(
                type, 
                variableDeclarationStatement.Variable.TypeNode.StartLine, 
                variableDeclarationStatement.Variable.TypeNode.StartColumn,
                variableDeclarationStatement.Variable.TypeNode.EndLine,
                variableDeclarationStatement.Variable.TypeNode.EndColumn
            );
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(modulePart, c, localSymbolTable, ref expr);
                variableDeclarationStatement.InitialValue = expr;
                AnalyzeTypes(modulePart, variableDeclarationStatement.Variable.TypeNode, expr.ReturnTypeNode!);
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
                throw new RedefinedException(modulePart.Source, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.StartLine, variableDeclarationStatement.Variable.StartColumn, variableDeclarationStatement.Variable.EndLine, variableDeclarationStatement.Variable.EndColumn);
            }
        }

        private bool AnalyzeTypes(ModulePart modulePart, TypeNode leftNode, TypeNode rightNode)
        {
            Type left = GetTypeFromTypeNode(modulePart, leftNode);
            Type right = GetTypeFromTypeNode(modulePart, rightNode);
            var sameName = right.FullName == left.FullName;
            var isSubClass = right.IsSubclassOf(left);
            var isAssignable = right.IsAssignableTo(left);
            if (!sameName && !isSubClass && !isAssignable)
            {
                throw new TypeDiscrepancyException(modulePart.Source, leftNode, rightNode, leftNode.StartLine, leftNode.StartColumn, leftNode.EndLine, leftNode.EndColumn);
            }
            return true;
        }

        private TypeNode AnalyzeExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression _: return AnalyzeVariableExpression(modulePart, c, localSymbolTable, ref expression);
                case NewArrayExpression newArrayExpression: return AnalyzeNewArrayExpression(modulePart, c, localSymbolTable, ref newArrayExpression);
                case InlineConstructionExpression inlineConstructionExpression: return AnalyzeInlineConstructionExpression(modulePart, c, localSymbolTable, inlineConstructionExpression);
                case ConstructorCallExpression constructorCallExpression: return AnalyzeConstructorCallExpression(modulePart, c, localSymbolTable, constructorCallExpression);
                case StandardMethodCallExpression standardMethodCallExpression: return AnalyzeStandardMethodCallExpression(modulePart, c, localSymbolTable, ref standardMethodCallExpression);
                case StaticMethodCallExpression staticMethodCallExpression: return AnalyzeStaticMethodCallExpression(modulePart, c, localSymbolTable, staticMethodCallExpression);
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression: 
                    var type = AnalyzeAmbiguousMethodCallExpression(modulePart, c, localSymbolTable, ref ambiguousMethodCallExpression);
                    expression = ambiguousMethodCallExpression;
                    return type;
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(modulePart, c, localSymbolTable,  binaryExpression);
                case ArrayIndexingExpression arrayIndexingExpression: return AnalyzeArrayIndexingExpression(modulePart, c, localSymbolTable, arrayIndexingExpression);
                case StandardFieldAccessmentExpression standardFieldAccessmentExpression: return AnalyzeStandardFieldAccessmentExpression(modulePart, c, localSymbolTable, standardFieldAccessmentExpression);
                case StaticFieldAccessmentExpression staticFieldAccessmentExpression: return AnalyzeStaticFieldAccessmentExpression(modulePart, localSymbolTable, staticFieldAccessmentExpression);
                case BoxExpression boxExpression: return AnalyzeBoxExpression(modulePart, c, localSymbolTable, boxExpression);
                case ThisExpression thisExpression: return AnalyzeThisExpression(modulePart, thisExpression);
                case DuplicateExpression duplicateExpression: return AnalyzeDuplicateExpression(modulePart, duplicateExpression);
                default: throw new NotImplementedException();
            }
        }

        private TypeNode AnalyzeDuplicateExpression(ModulePart modulePart, DuplicateExpression duplicateExpression)
        {
            var returnType = GetTypeFromTypeNode(modulePart, duplicateExpression.ReturnTypeNode!);
            duplicateExpression.ReturnTypeNode = CreateTypeNodeFromType(
                returnType, 
                duplicateExpression.StartLine, 
                duplicateExpression.StartColumn,
                duplicateExpression.EndLine,
                duplicateExpression.EndColumn);
            return duplicateExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeThisExpression(ModulePart modulePart, ThisExpression thisExpression)
        {
            var returnType = GetTypeFromTypeNode(modulePart, thisExpression.ReturnTypeNode!);
            thisExpression.ReturnTypeNode = CreateTypeNodeFromType(
                returnType, 
                thisExpression.StartLine, 
                thisExpression.StartColumn,
                thisExpression.EndLine,
                thisExpression.EndColumn);
            return thisExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeBoxExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, BoxExpression boxExpression)
        {
            var expr = boxExpression.Expression;
            var type = AnalyzeExpression(modulePart, c, localSymbolTable, ref expr);
            boxExpression.Expression = expr;
            boxExpression.ReturnTypeNode = type;
            return type;
        }

        private TypeNode AnalyzeStaticFieldAccessmentExpression(ModulePart modulePart, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StaticFieldAccessmentExpression staticFieldAccessmentExpression)
        {
            var fieldType = GetTypeFromTypeNode(modulePart, staticFieldAccessmentExpression.TypeNode);
            staticFieldAccessmentExpression.TypeNode = CreateTypeNodeFromType(
                fieldType,
                staticFieldAccessmentExpression.StartLine,
                staticFieldAccessmentExpression.StartColumn,
                staticFieldAccessmentExpression.EndLine,
                staticFieldAccessmentExpression.EndColumn
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
                        modulePart.Source,
                        staticFieldAccessmentExpression!.FieldName,
                        staticFieldAccessmentExpression.StartLine,
                        staticFieldAccessmentExpression.StartColumn,
                        staticFieldAccessmentExpression.EndLine,
                        staticFieldAccessmentExpression.EndColumn
                    );
                staticFieldAccessmentExpression.FieldInfo = fieldInfo;
            }
            else
            {
                staticFieldAccessmentExpression.FieldInfo = fieldType.GetField(staticFieldAccessmentExpression.FieldName, BindingFlags.Public);
                if (staticFieldAccessmentExpression.FieldInfo == null)
                    throw new InvalidFieldAccessmentException(
                        modulePart.Source,
                        staticFieldAccessmentExpression!.FieldName,
                        staticFieldAccessmentExpression.StartLine,
                        staticFieldAccessmentExpression.StartColumn,
                        staticFieldAccessmentExpression.EndLine,
                        staticFieldAccessmentExpression.EndColumn
                    );
            }
            staticFieldAccessmentExpression.ReturnTypeNode = CreateTypeNodeFromType(
                staticFieldAccessmentExpression.FieldInfo!.FieldType,
                staticFieldAccessmentExpression.StartLine,
                staticFieldAccessmentExpression.StartColumn,
                staticFieldAccessmentExpression.EndLine,
                staticFieldAccessmentExpression.EndColumn
            );
            return staticFieldAccessmentExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeStandardFieldAccessmentExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StandardFieldAccessmentExpression standardFieldAccessmentExpression)
        {
            var fieldObject = standardFieldAccessmentExpression.Object;
            var objectTypeNode = AnalyzeExpression(modulePart, c, localSymbolTable, ref fieldObject);
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
                        modulePart.Source,
                        standardFieldAccessmentExpression!.FieldName,
                        standardFieldAccessmentExpression.StartLine,
                        standardFieldAccessmentExpression.StartColumn,
                        standardFieldAccessmentExpression.EndLine,
                        standardFieldAccessmentExpression.EndColumn
                    );
                standardFieldAccessmentExpression.FieldInfo = fieldInfo;
            }
            else
            {
                var fieldObjectType = GetTypeFromTypeNode(modulePart, fieldObject.ReturnTypeNode!);
                if (standardFieldAccessmentExpression.FieldInfo == null)
                    standardFieldAccessmentExpression.FieldInfo = fieldObjectType.GetField(standardFieldAccessmentExpression.FieldName, BindingFlags.Public);
                if (standardFieldAccessmentExpression.FieldInfo == null)
                    throw new InvalidFieldAccessmentException(
                        modulePart.Source,
                        standardFieldAccessmentExpression!.FieldName,
                        standardFieldAccessmentExpression.StartLine,
                        standardFieldAccessmentExpression.StartColumn,
                        standardFieldAccessmentExpression.EndLine,
                        standardFieldAccessmentExpression.EndColumn
                    );
            }
            standardFieldAccessmentExpression.ReturnTypeNode = CreateTypeNodeFromType(
                standardFieldAccessmentExpression.FieldInfo!.FieldType,
                standardFieldAccessmentExpression.StartLine,
                standardFieldAccessmentExpression.StartColumn,
                standardFieldAccessmentExpression.EndLine,
                standardFieldAccessmentExpression.EndColumn
            );
            return standardFieldAccessmentExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeConstructorCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ConstructorCallExpression constructorCallExpression)
        {
            List<Type> argTypes = [];
            for (int i = 0; i < constructorCallExpression.Arguments.Length; i++)
            {
                var argTypeNode = AnalyzeExpression(modulePart, c, localSymbolTable, ref constructorCallExpression.Arguments[i]);
                argTypes.Add(GetTypeFromTypeNode(modulePart, argTypeNode));
            }
            var returnType = GetTypeFromTypeNode(modulePart, constructorCallExpression.ReturnTypeNode!);
            if (moduleConstructors.TryGetValue((returnType.FullName!, [..argTypes]), out var constructorInfo))
            {
                constructorCallExpression.ConstructorInfo = constructorInfo;
            }
            else
            {
                try
                {
                    constructorCallExpression.ConstructorInfo = returnType.GetConstructor([..argTypes]);
                }
                catch (Exception) { }
            }
            if (constructorCallExpression.ConstructorInfo == null || constructorCallExpression.ConstructorInfo!.IsPrivate)
                throw new UnresolvedConstructorException(
                    modulePart.Source,
                    returnType.FullName!,
                    [],
                    constructorCallExpression.StartLine,
                    constructorCallExpression.StartColumn,
                    constructorCallExpression.EndLine,
                    constructorCallExpression.EndColumn
                );
            constructorCallExpression.ReturnTypeNode = CreateTypeNodeFromType(
                returnType,
                constructorCallExpression.ReturnTypeNode!.StartLine,
                constructorCallExpression.ReturnTypeNode!.StartColumn,
                constructorCallExpression.ReturnTypeNode!.EndLine,
                constructorCallExpression.ReturnTypeNode!.EndColumn
            );
            return constructorCallExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeInlineConstructionExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, InlineConstructionExpression inlineConstructionExpression)
        {
            foreach(var statement in inlineConstructionExpression.FieldInitializations)
            {
                AnalyzeAssignmentStatement(modulePart, c, localSymbolTable, statement);
            }
            var returnType = GetTypeFromTypeNode(modulePart, inlineConstructionExpression.ReturnTypeNode!);
            if (moduleConstructors.TryGetValue((returnType.FullName!, []), out var constructorInfo))
            {
                inlineConstructionExpression.ConstructorInfo = constructorInfo;
            }
            else
            {
                try
                {
                    inlineConstructionExpression.ConstructorInfo = returnType.GetConstructor([]);
                }
                catch (Exception) { }
            }
            if (inlineConstructionExpression.ConstructorInfo == null || inlineConstructionExpression.ConstructorInfo!.IsPrivate) 
                throw new UnresolvedConstructorException(
                    modulePart.Source, 
                    returnType.FullName!, 
                    [], 
                    inlineConstructionExpression.StartLine, 
                    inlineConstructionExpression.StartColumn,
                    inlineConstructionExpression.EndLine,
                    inlineConstructionExpression.EndColumn
                );
            inlineConstructionExpression.ReturnTypeNode = CreateTypeNodeFromType(
                returnType,
                inlineConstructionExpression.ReturnTypeNode!.StartLine,
                inlineConstructionExpression.ReturnTypeNode.StartColumn,
                inlineConstructionExpression.ReturnTypeNode.EndLine,
                inlineConstructionExpression.ReturnTypeNode.EndColumn
            );
            return inlineConstructionExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeArrayIndexingExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ArrayIndexingExpression arrayIndexingExpression)
        {
            var arrayNode = arrayIndexingExpression.Array;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref arrayNode);
            arrayIndexingExpression.Array = arrayNode;
            var arrayType = GetTypeFromTypeNode(modulePart, arrayNode.ReturnTypeNode!);
            if (!arrayType.IsArray)
            {
                throw new IndexingOnNonArrayException(modulePart.Source, arrayNode.ReturnTypeNode!.FullName, arrayIndexingExpression.StartLine, arrayIndexingExpression.StartColumn, arrayIndexingExpression.EndLine, arrayIndexingExpression.EndColumn);
            }
            var indexNode = arrayIndexingExpression.Index;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref indexNode);
            arrayIndexingExpression.Index = indexNode;
            if (indexNode.ReturnTypeNode!.CLRFullName != PrimitiveType.Int32 && indexNode.ReturnTypeNode.CLRFullName != PrimitiveType.Int64)
            {
                throw new InvalidArrayIndexingTypeException(modulePart.Source, indexNode.ReturnTypeNode.FullName, indexNode.StartLine, indexNode.StartColumn, indexNode.EndLine, indexNode.EndColumn);
            }
            arrayIndexingExpression.ReturnTypeNode = CreateTypeNodeFromType(
                arrayType.GetElementType()!, 
                arrayIndexingExpression.StartLine, 
                arrayIndexingExpression.StartColumn,
                arrayIndexingExpression.EndLine,
                arrayIndexingExpression.EndColumn);
            return arrayIndexingExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeNewArrayExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref NewArrayExpression newArrayExpression)
        {
            int? size = null;
            Type type = GetTypeFromTypeNode(modulePart, newArrayExpression.ReturnTypeNode!);
            newArrayExpression.ReturnTypeNode = CreateTypeNodeFromType(
                type, 
                newArrayExpression.ReturnTypeNode!.StartLine, 
                newArrayExpression.ReturnTypeNode.StartColumn,
                newArrayExpression.ReturnTypeNode.EndLine,
                newArrayExpression.ReturnTypeNode.EndColumn);

            // Analyze size expression if not null
            if (newArrayExpression.Size != null)
            {
                ExpressionNode sizeNode = newArrayExpression.Size;
                var sizeType = AnalyzeExpression(modulePart, c, localSymbolTable, ref sizeNode);
                newArrayExpression.Size = sizeNode;
                if (sizeType.CLRFullName != PrimitiveType.Int32 && sizeType.CLRFullName != PrimitiveType.Int64)
                {
                    throw new InvalidArraySizeTypeException(modulePart.Source, sizeType.FullName, sizeNode.StartLine, sizeNode.StartColumn, sizeNode.EndLine, sizeNode.EndColumn);
                }
                size = GetValueIfLiteral(sizeNode);
                if (size != null && size < 1)
                {
                    throw new NegativeArraySizeException(modulePart.Source, size.Value, sizeNode.StartLine, sizeNode.StartColumn, sizeNode.EndLine, sizeNode.EndColumn);
                }
            }
            // Analyze elements
            for (int i = 0; i < newArrayExpression.Elements.Length; i++)
            {
                TypeNode expressionTypeNode = AnalyzeExpression(modulePart, c, localSymbolTable, ref newArrayExpression.Elements[i]);
                Type expressionType = GetTypeFromTypeNode(modulePart, expressionTypeNode);
                Type objectType = module.CoreAssembly!.GetType("System.Object")!;
                TypeNode elementTypeNode = ((ArrayType)newArrayExpression.ReturnTypeNode).ElementTypeNode;
                Type elementType = GetTypeFromTypeNode(modulePart, elementTypeNode);
                if (expressionType.IsValueType && elementType == objectType)
                {
                    newArrayExpression.Elements[i] = new BoxExpression
                    {
                        ReturnTypeNode = newArrayExpression.Elements[i].ReturnTypeNode,
                        Expression = newArrayExpression.Elements[i],
                        StartLine = newArrayExpression.Elements[i].StartLine,
                        StartColumn = newArrayExpression.Elements[i].StartColumn,
                        EndLine = newArrayExpression.Elements[i].EndLine,
                        EndColumn = newArrayExpression.Elements[i].EndColumn
                    };
                }
                else
                {
                    AnalyzeTypes(modulePart, elementTypeNode, expressionTypeNode);
                }
            }
            // Check if size isn't specified but no initialization
            if (newArrayExpression.Size == null && newArrayExpression.Elements.Length == 0)
            {
                throw new NoSizeArrayWithoutInitializationException(
                    modulePart.Source, 
                    newArrayExpression.ReturnTypeNode!.StartLine, 
                    newArrayExpression.ReturnTypeNode!.StartColumn,
                    newArrayExpression.ReturnTypeNode!.EndLine,
                    newArrayExpression.ReturnTypeNode!.EndColumn);
            }
            // Check if size is specified but number of elements is unequal
            if (size != null && size != newArrayExpression.Elements.Length && newArrayExpression.Elements.Length != 0)
            {
                throw new ArraySizeDiscrepancyException(
                    modulePart.Source, 
                    size.Value, 
                    newArrayExpression.ReturnTypeNode!.StartLine, 
                    newArrayExpression.ReturnTypeNode.StartColumn,
                    newArrayExpression.ReturnTypeNode.EndLine,
                    newArrayExpression.ReturnTypeNode.EndColumn, 
                    newArrayExpression.Elements.Length, 
                    newArrayExpression.Elements[0].StartLine, 
                    newArrayExpression.Elements[0].StartColumn,
                    newArrayExpression.Elements.Last().EndLine,
                    newArrayExpression.Elements.Last().EndColumn
                    );
            }
            // Ignore size is specified but no initialization but 

            // Ignore if size expression has a variable expression or call expression (size == null && newArrayExpression.Size != null)

            // If size isn't specified but there is initialization (checked from before), set the size for assembler later
            newArrayExpression.Size ??= new LiteralExpression { 
                    ReturnTypeNode = CreateTypeNodeFromType(
                        module.CoreAssembly!.GetType(PrimitiveType.Int32)!,
                        newArrayExpression.ReturnTypeNode!.StartLine,
                        newArrayExpression.ReturnTypeNode!.StartColumn,
                        newArrayExpression.ReturnTypeNode.EndLine,
                        newArrayExpression.ReturnTypeNode.EndColumn
                    ),
                    StartLine = newArrayExpression.ReturnTypeNode!.StartLine, 
                    StartColumn = newArrayExpression.ReturnTypeNode!.StartColumn, 
                    EndLine = newArrayExpression.ReturnTypeNode!.EndLine,
                    EndColumn = newArrayExpression.ReturnTypeNode!.EndColumn, 
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

        private void AnalyzeArguments<T>(ModulePart modulePart, ref T methodCallExpression) where T : AmbiguousMethodCallExpression
        {
            var parameters = methodCallExpression.MethodInfo!.GetParameters();
            var objectType = module.CoreAssembly!.GetType("System.Object");
            for (int i = 0; i < parameters.Length; i++)
            {
                if (
                    parameters[i].ParameterType == objectType &&
                    GetTypeFromTypeNode(modulePart, methodCallExpression.Arguments[i].ReturnTypeNode!).IsValueType
                )
                {
                    methodCallExpression.Arguments[i] = new BoxExpression
                    {
                        ReturnTypeNode = methodCallExpression.Arguments[i].ReturnTypeNode,
                        Expression = methodCallExpression.Arguments[i],
                        StartLine = methodCallExpression.Arguments[i].StartLine,
                        StartColumn = methodCallExpression.Arguments[i].StartColumn,
                        EndLine = methodCallExpression.Arguments[i].EndLine,
                        EndColumn = methodCallExpression.Arguments[i].EndColumn
                    };
                }
            }
        }

        private TypeNode AnalyzeStandardMethodCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref StandardMethodCallExpression standardMethodCallExpression)
        {
            if (currentMethod == null)
            {
                throw new MethodCallFromOutsideException(modulePart.Source, standardMethodCallExpression.StartLine, standardMethodCallExpression.StartColumn, standardMethodCallExpression.EndLine, standardMethodCallExpression.EndColumn);
            }
            var objec = standardMethodCallExpression.Object;
            var typeNode = AnalyzeExpression(modulePart, c, localSymbolTable, ref objec);
            var methodInfo = standardMethodCallExpression.MethodInfo;
            standardMethodCallExpression.Object = objec;
            if (
                currentMethod.GetMethodInfo()!.IsStatic &&
                string.Join(".", [c.Name, standardMethodCallExpression.MethodName]) ==
                string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.MethodName])
            )
            {
                throw new StaticIllegalAccessmentException(
                    modulePart.Source,
                    string.Join("::", [c.Name, standardMethodCallExpression.MethodName]),
                    standardMethodCallExpression.StartLine,
                    standardMethodCallExpression.StartColumn,
                    standardMethodCallExpression.EndLine,
                    standardMethodCallExpression.EndColumn
                );
            }
            var methodFullName = string.Join(".", [typeNode.CLRFullName, standardMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(
                modulePart,
                c, 
                localSymbolTable, 
                ref standardMethodCallExpression, 
                methodFullName, 
                GetTypeFromTypeNode(modulePart, typeNode)
                );
            standardMethodCallExpression.ReturnTypeNode = CreateTypeNodeFromType(
                GetReturnTypeFromMethodBase(standardMethodCallExpression.MethodInfo!)!,
                standardMethodCallExpression.StartLine,
                standardMethodCallExpression.StartColumn,
                standardMethodCallExpression.EndLine,
                standardMethodCallExpression.EndColumn
            );
            return standardMethodCallExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeStaticMethodCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            staticMethodCallExpression.TypeNode.CLRType = GetTypeFromTypeNode(modulePart, staticMethodCallExpression.TypeNode);
            var methodFullName = string.Join(".", [staticMethodCallExpression.TypeNode.CLRFullName, staticMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(modulePart, c, localSymbolTable, ref staticMethodCallExpression, methodFullName, staticMethodCallExpression.TypeNode.CLRType);
            if (!(staticMethodCallExpression.MethodInfo as MethodInfo)!.IsStatic)
                throw new StandardMethodCallLikeStaticMethodException(
                    modulePart.Source,
                    staticMethodCallExpression.StartLine,
                    staticMethodCallExpression.StartColumn,
                    staticMethodCallExpression.EndLine,
                    staticMethodCallExpression.EndColumn);
            staticMethodCallExpression.ReturnTypeNode = CreateTypeNodeFromType(
                GetReturnTypeFromMethodBase(staticMethodCallExpression.MethodInfo!)!, 
                staticMethodCallExpression.StartLine, 
                staticMethodCallExpression.StartColumn,
                staticMethodCallExpression.EndLine,
                staticMethodCallExpression.EndColumn
            );
            return staticMethodCallExpression.ReturnTypeNode;
        }

        /// <summary>
        /// Will mutate the expression into a StandardMethodCallExpression or a StaticMethodCallExpression
        /// </summary>
        /// <param name="modulePart"></param>
        /// <param name="c"></param>
        /// <param name="localSymbolTable"></param>
        /// <param name="ambiguousMethodCallExpression"></param>
        /// <returns></returns>
        /// <exception cref="MethodCallFromOutsideException"></exception>
        /// <exception cref="StaticIllegalAccessmentException"></exception>
        private TypeNode AnalyzeAmbiguousMethodCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            if (currentMethod == null)
            {
                throw new MethodCallFromOutsideException(modulePart.Source, ambiguousMethodCallExpression.StartLine, ambiguousMethodCallExpression.StartColumn, ambiguousMethodCallExpression.EndLine, ambiguousMethodCallExpression.EndColumn);
            }
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(modulePart, c, localSymbolTable, ref ambiguousMethodCallExpression, methodFullName);
            if (ambiguousMethodCallExpression.MethodInfo!.IsStatic)
            {
                var typeNode = CreateTypeNodeFromType(
                    ambiguousMethodCallExpression.MethodInfo.DeclaringType!,
                    ambiguousMethodCallExpression.StartLine,
                    ambiguousMethodCallExpression.StartColumn,
                    ambiguousMethodCallExpression.EndLine,
                    ambiguousMethodCallExpression.EndColumn
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
                                            ambiguousMethodCallExpression.StartLine,
                                            ambiguousMethodCallExpression.StartColumn,
                                            ambiguousMethodCallExpression.EndLine,
                                            ambiguousMethodCallExpression.EndColumn
                                            ),
                        MethodInfo = ambiguousMethodCallExpression.MethodInfo,
                        StartLine = ambiguousMethodCallExpression.StartLine,
                        StartColumn = ambiguousMethodCallExpression.StartColumn,
                        EndLine = ambiguousMethodCallExpression.EndLine,
                        EndColumn = ambiguousMethodCallExpression.EndColumn
                    },
                    _ => throw new("Internal error"),
                };
            }
            else
            {
                if (currentMethod.GetMethodInfo()!.IsStatic)
                {
                    throw new StaticIllegalAccessmentException(
                        modulePart.Source, 
                        string.Join("::", [c.Name, ambiguousMethodCallExpression.MethodName]), 
                        ambiguousMethodCallExpression.StartLine, 
                        ambiguousMethodCallExpression.StartColumn,
                        ambiguousMethodCallExpression.EndLine,
                        ambiguousMethodCallExpression.EndColumn
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
                            StartLine = ambiguousMethodCallExpression.StartLine,
                            StartColumn = ambiguousMethodCallExpression.StartColumn,
                            EndLine = ambiguousMethodCallExpression.EndLine,
                            EndColumn = ambiguousMethodCallExpression.EndColumn
                        },
                        StartLine = ambiguousMethodCallExpression.StartLine,
                        StartColumn = ambiguousMethodCallExpression.StartColumn,
                        EndLine = ambiguousMethodCallExpression.EndLine,
                        EndColumn = ambiguousMethodCallExpression.EndColumn
                    },
                    MethodName = ambiguousMethodCallExpression.MethodName,
                    ReturnTypeNode = CreateTypeNodeFromType(
                        GetReturnTypeFromMethodBase(ambiguousMethodCallExpression.MethodInfo)!, 
                        ambiguousMethodCallExpression.StartLine, 
                        ambiguousMethodCallExpression.StartColumn,
                        ambiguousMethodCallExpression.EndLine,
                        ambiguousMethodCallExpression.EndColumn
                    ),
                    MethodInfo = ambiguousMethodCallExpression.MethodInfo,
                    StartLine = ambiguousMethodCallExpression.StartLine,
                    StartColumn = ambiguousMethodCallExpression.StartColumn,
                    EndLine = ambiguousMethodCallExpression.EndLine,
                    EndColumn = ambiguousMethodCallExpression.EndColumn
                };
            }
            return ambiguousMethodCallExpression.ReturnTypeNode!;
        }

        private void AnalyzeMethodCallExpression<T>(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref T methodCallExpression, string methodFullName, Type? type = null) where T : AmbiguousMethodCallExpression
        {
            // Analyze the argument expressions
            List<Type> argumentTypes = [];
            for (int i = 0; i < methodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(modulePart, c, localSymbolTable, ref methodCallExpression.Arguments[i]);
                argumentTypes.Add(GetTypeFromTypeNode(modulePart, typ));
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
                    bool isFromSameOrParentType;
                    if (type != null)
                    {
                        try
                        {
                            isFromSameOrParentType = AnalyzeTypes(
                                modulePart,
                            CreateTypeNodeFromType(
                                typeTable.GetValueOrDefault(c.Name)!, 
                                methodCallExpression.StartLine, 
                                methodCallExpression.StartColumn,
                                methodCallExpression.EndLine,
                                methodCallExpression.EndColumn),
                        CreateTypeNodeFromType(
                            type!, 
                            methodCallExpression.StartLine, 
                            methodCallExpression.StartColumn,
                            methodCallExpression.EndLine,
                            methodCallExpression.EndColumn)
                            );
                        } catch(Exception) {
                            isFromSameOrParentType = false;
                        }
                    }
                    else
                    {
                        isFromSameOrParentType = true;
                    }
                    if (!methodInfo.IsPublic && !isFromSameOrParentType
                    ) throw new InvalidMethodCallException(
                        modulePart.Source,
                        methodCallExpression.MethodName,
                        [.. argumentTypes],
                        methodCallExpression.StartLine,
                        methodCallExpression.StartColumn,
                        methodCallExpression.EndLine,
                        methodCallExpression.EndColumn
                    );
                    methodCallExpression.MethodInfo = methodInfo;
                }
                else
                {
                    if (type as TypeBuilder == null)
                    {
                        methodCallExpression.MethodInfo = type?.GetMethod(
                        methodCallExpression.MethodName,
                        [.. argumentTypes]
                        ) ?? throw new InvalidMethodCallException(
                            modulePart.Source,
                            methodCallExpression.MethodName,
                            [.. argumentTypes],
                            methodCallExpression.StartLine,
                            methodCallExpression.StartColumn,
                            methodCallExpression.EndLine,
                            methodCallExpression.EndColumn
                        );

                        if (!methodCallExpression.MethodInfo.IsPublic) throw new InvalidMethodCallException(
                            modulePart.Source,
                            methodCallExpression.MethodName,
                            [.. argumentTypes],
                            methodCallExpression.StartLine,
                            methodCallExpression.StartColumn,
                            methodCallExpression.EndLine,
                            methodCallExpression.EndColumn
                        );
                    }
                    else throw new InvalidMethodCallException(
                            modulePart.Source,
                            methodCallExpression.MethodName,
                            [.. argumentTypes],
                            methodCallExpression.StartLine,
                            methodCallExpression.StartColumn,
                            methodCallExpression.EndLine,
                            methodCallExpression.EndColumn
                        );
                }
            }
            // Analyze the argument again in case of needing boxing
            AnalyzeArguments(modulePart, ref methodCallExpression);
        }

        private TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                string[] namespaceAndName = PrimitiveType.String.Split(".");
                literalExpression.ReturnTypeNode = new NamedType { 
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.String) ?? throw new("Internal error"), 
                    StartLine = literalExpression.StartLine, 
                    StartColumn = literalExpression.StartColumn,
                    EndLine = literalExpression.EndLine,
                    EndColumn = literalExpression.EndColumn
                };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                string[] namespaceAndName = PrimitiveType.Int64.Split(".");
                literalExpression.ReturnTypeNode = new NamedType { 
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int64) ?? throw new("Internal error"), 
                    StartLine = literalExpression.StartLine, 
                    StartColumn = literalExpression.StartColumn,
                    EndLine = literalExpression.EndLine,
                    EndColumn = literalExpression.EndColumn 
                };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int32.TryParse(literalExpression.Value, out _))
            {
                string[] namespaceAndName = PrimitiveType.Int32.Split(".");
                literalExpression.ReturnTypeNode = new NamedType {
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int32) ?? throw new("Internal error"), 
                    StartLine = literalExpression.StartLine, 
                    StartColumn = literalExpression.StartColumn,
                    EndLine = literalExpression.EndLine,
                    EndColumn = literalExpression.EndColumn 
                };
            }
            else if (literalExpression.Value == "đúng" || literalExpression.Value == "sai")
            {
                string[] namespaceAndName = PrimitiveType.Boolean.Split(".");
                literalExpression.ReturnTypeNode = new NamedType
                {
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Boolean) ?? throw new("Internal error"),
                    StartLine = literalExpression.StartLine,
                    StartColumn = literalExpression.StartColumn,
                    EndLine = literalExpression.EndLine,
                    EndColumn = literalExpression.EndColumn
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
        private TypeNode AnalyzeVariableExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionNode expression)
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
                    StartLine = variableExpression.StartLine,
                    StartColumn = variableExpression.StartColumn,
                    EndLine = variableExpression.EndLine,
                    EndColumn = variableExpression.EndColumn,
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
                            StartLine = expression.StartLine,
                            StartColumn = expression.StartColumn,
                            EndLine = expression.EndLine,
                            EndColumn = expression.EndColumn
                        },
                        FieldName = field.Variable.Name,
                        FieldInfo = field.FieldInfo,
                        StartLine = expression.StartLine,
                        StartColumn = expression.StartColumn,
                        EndLine = expression.EndLine,
                        EndColumn = expression.EndColumn
                    };
                    AnalyzeStaticFieldAccessmentExpression(modulePart, localSymbolTable, (StaticFieldAccessmentExpression)expression);
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
                                StartLine = expression.StartLine,
                                StartColumn = expression.StartColumn,
                                EndLine = expression.EndLine,
                                EndColumn = expression.EndColumn
                            },
                            StartLine = expression.StartLine,
                            StartColumn = expression.StartColumn,
                            EndLine = expression.EndLine,
                            EndColumn = expression.EndColumn,
                        },
                        FieldName = field.Variable.Name,
                        FieldInfo = field.FieldInfo,
                        StartLine = expression.StartLine,
                        StartColumn = expression.StartColumn,
                        EndLine = expression.EndLine,
                        EndColumn = expression.EndColumn
                    };
                    AnalyzeStandardFieldAccessmentExpression(modulePart, c, localSymbolTable, (StandardFieldAccessmentExpression)expression);
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
                throw new UnresolvedIdentifierException(modulePart.Source, variableExpression.Name, variableExpression.StartLine, variableExpression.StartColumn, variableExpression.EndLine, variableExpression.EndColumn);
            }
        }

        private TypeNode AnalyzeBinaryExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(modulePart, c, localSymbolTable,  ref left);
            var rightType = AnalyzeExpression(modulePart, c, localSymbolTable,  ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = leftType.CLRFullName;
            var rightTypeFullName = rightType.CLRFullName;
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(modulePart.Source, leftType, rightType, binaryExpression.StartLine, binaryExpression.StartColumn, binaryExpression.EndLine, binaryExpression.EndColumn);
            
            if (binaryExpression.Operator == "và" || 
                binaryExpression.Operator == "hoặc" ||
                binaryExpression.Operator == "=" ||
                binaryExpression.Operator == "!=" ||
                binaryExpression.Operator == "<" ||
                binaryExpression.Operator == "<=" ||
                binaryExpression.Operator == ">" ||
                binaryExpression.Operator == ">="
            )
            {
                string[] namespaceAndName = PrimitiveType.Boolean.Split(".");
                binaryExpression.ReturnTypeNode = new NamedType
                {
                    Namespaces = namespaceAndName[0..^1],
                    Name = namespaceAndName[^1],
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Boolean),
                    StartLine = binaryExpression.StartLine,
                    StartColumn = binaryExpression.StartColumn,
                    EndLine = binaryExpression.EndLine,
                    EndColumn = binaryExpression.EndColumn
                };
                return binaryExpression.ReturnTypeNode;
            }
            else binaryExpression.ReturnTypeNode = rightType;
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

        private Type GetTypeFromTypeNode(ModulePart modulePart, TypeNode typeNode)
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
                    var elementType = GetTypeFromTypeNode(modulePart, arrayTypeNode.ElementTypeNode);
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
                    var namespaces = modulePart.UsingNamespaces
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
                            throw new AmbiguousTypeUsage(modulePart.Source, tn, ftn, stn, typeNode.StartLine, typeNode.StartColumn, typeNode.EndLine, typeNode.EndColumn);
                        }
                        if (tempType == null && type != null) continue;
                        type = tempType;
                    }
                    if (type == null)
                    {
                        throw new UnresolvedTypeException(modulePart.Source, typeNode.FullName, typeNode.StartLine, typeNode.StartColumn, typeNode.EndLine, typeNode.EndColumn);
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

        static public TypeNode CreateTypeNodeFromType(Type type, int startLine, int startColumn, int endLine, int endColumn)
        {
            var namespaces = type.Namespace?.Split(".") ?? [];
            if (type.IsArray)
            {
                Type elementType = type.GetElementType()!;
                return new ArrayType
                {
                    ElementTypeNode = CreateTypeNodeFromType(elementType, startLine, startColumn, endLine, endColumn),
                    Namespaces = namespaces,
                    Name = type.Name,
                    StartLine = startLine,
                    StartColumn = startColumn,
                    EndLine = endLine,
                    EndColumn = endColumn,
                };
            }
            return new NamedType { 
                CLRType = type,
                Namespaces = namespaces,
                Name = type.Name,
                StartLine = startLine,
                StartColumn = startColumn,
                EndLine = endLine,
                EndColumn = endColumn
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
