using NEN.Exceptions;
using NEN.Types;
using System.ComponentModel.Design;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

namespace NEN
{
    public partial class StaticAnalyzer(string assemblyName, ModulePart[] moduleParts, string[] assemblyPaths)
    {
        private readonly Dictionary<string, Type> typeTable = [];
        private readonly Dictionary<(string, Type[]), MethodInfo> moduleMethods = new(new MethodSignatureComparer());
        private readonly Dictionary<(string, Type[]), ConstructorInfo> moduleConstructors = new(new MethodSignatureComparer());
        private readonly Dictionary<string, FieldInfo> moduleFields = [];
        private readonly Types.Module module = new() { Name = assemblyName, ModuleParts = moduleParts};
        private Types.MethodBase? currentMethod = null;

        public Types.Module Analyze()
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
                // Define every field in the classes
                foreach (var c in modulePart.Classes)
                {
                    foreach (var field in c.Fields)
                    {
                        if (c.Fields.Where(f => f.Variable.Name == field.Variable.Name).ToArray().Length > 1)
                        {
                            throw new RedefinedException(modulePart.Source, field.Variable.Name, field.Variable.Line, field.Variable.Column);
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
                    // Decide if type needs to generate a default constructor
                    var fieldsWithInitialization = c.Fields.Where(f => f.InitialValue != null).ToArray();
                    if (c.Constructors != null && fieldsWithInitialization.Length > 0)
                        throw new FieldInitializationOutsideDefaultConstructorException(
                            modulePart.Source,
                            fieldsWithInitialization.First().Line,
                            fieldsWithInitialization.First().Column
                        );
                    if (c.Constructors == null && fieldsWithInitialization.Length > 0) GenerateDefaultConstructor(modulePart, c);
                }
            }
            // Declaring the methods in every classes in every module
            List<Dictionary<string, (TypeNode, LocalBuilder)>[]> lsLsSt = [];
            foreach (var modulePart in module.ModuleParts)
            {
                // Define every method in the classes
                foreach (var c in modulePart.Classes)
                {
                    List<Dictionary<string, (TypeNode, LocalBuilder)>> lsSt = [];
                    foreach (var method in c.Methods)
                    {
                        lsSt.Add(DefineMethod(modulePart, c, method));
                    }
                    lsLsSt.Add([.. lsSt]);
                }
            }
            int i = 0;
            foreach (var modulePart in module.ModuleParts)
            {
                // Analyze the method bodies in each class
                for (int j = 0; j < modulePart.Classes.Length; j++)
                {
                    AnalyzeClass(modulePart, modulePart.Classes[j], lsLsSt[i]);
                    i++;
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
                throw new RedefinedException(modulePart.Source, c.Name, c.Line, c.Column);
            }
        }

        private Dictionary<string, (TypeNode, LocalBuilder)> DefineMethod(ModulePart modulePart, ClassNode c, MethodNode method)
        {
            Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable = new();
            var type = GetTypeFromTypeNode(modulePart, method.ReturnTypeNode);
            method.ReturnTypeNode = CreateTypeNodeFromType(type, method.ReturnTypeNode.Line, method.ReturnTypeNode.Column);
            List<Type> paramTypes = [];
            foreach (var parameter in method.Parameters)
            {
                var paramType = GetTypeFromTypeNode(modulePart, parameter.TypeNode);
                parameter.TypeNode = CreateTypeNodeFromType(paramType, parameter.TypeNode.Line, parameter.TypeNode.Column);
                if (method.Parameters.Where(p => p.Name == parameter.Name).ToArray().Length > 1)
                {
                    throw new RedefinedException(modulePart.Source, parameter.Name, parameter.Line, parameter.Column);
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
                int index = method.MethodBuilder.IsStatic ? i : i + 1;
                ParameterBuilder p = method.MethodBuilder.DefineParameter(index, ParameterAttributes.None, method.Parameters[i].Name);
            }
            string methodFullName = string.Join('.', [c.Name, method.MethodName]);
            if (!moduleMethods.TryAdd((methodFullName, [.. paramTypes]), method.MethodBuilder))
            {
                throw new RedefinedException(modulePart.Source, methodFullName, method.Line, method.Column);
            }
            return localSymbolTable;
        }

        private void AnalyzeUsingNamespaceStatement(ModulePart modulePart, UsingNamespaceStatement usingNamespaceStatement)
        {
            Type? type = module.CoreAssembly!.GetType(string.Join(".", usingNamespaceStatement.Namespace));
            if (type != null)
            {
                throw new InvalidUsingStatement(modulePart.Source, string.Join("::", usingNamespaceStatement.Namespace), usingNamespaceStatement.Line, usingNamespaceStatement.Column);
            }
            if (!module.AvailableNamespaces.Contains(string.Join(".", usingNamespaceStatement.Namespace)))
            {
                throw new UnresolvedIdentifierException(
                    modulePart.Source, 
                    string.Join("::", usingNamespaceStatement.Namespace), 
                    usingNamespaceStatement.Line, 
                    usingNamespaceStatement.Column
                );
            }
            usingNamespaceStatement.IsResolved = true;
        }

        private void AnalyzeClass(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)>[] localSymbolTableList)
        {
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(modulePart, c, c.Methods[i], localSymbolTableList[i]);
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

        private void AnalyzeMethod(ModulePart modulePart, ClassNode c, MethodNode method, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable)
        {
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
            breakStatement.EndLabel = endLabel ?? throw new BreakOutsideLoopException(modulePart.Source, breakStatement.Line, breakStatement.Column);
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
                    condition.Line,
                    condition.Column
                );
            switch (currentMethod!.GetMethodInfo())
            {
                case MethodBuilder methodBuilder:
                    whileStatement.EndLabel = methodBuilder.GetILGenerator().DefineLabel();
                    break;
                default:
                    throw new NotImplementedException();
            }
            foreach(var statement in whileStatement.Body)
            {
                AnalyzeStatement(modulePart, c, new(localSymbolTable), statement, whileStatement.EndLabel);
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
                    condition.Line, 
                    condition.Column
                );
            foreach (var statement in ifStatement.IfClause)
            {
                AnalyzeStatement(modulePart, c, new(localSymbolTable), statement, endLabel);
            }
            foreach (var statement in ifStatement.ElseClause)
            {
                AnalyzeStatement(modulePart, c, new(localSymbolTable), statement, endLabel);
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
                returnStatement.Line, 
                returnStatement.Column
            );
            if (returnStatement.Expression != null)
            {
                var expression = returnStatement.Expression;
                type = AnalyzeExpression(modulePart, c, localSymbolTable, ref expression);
                returnStatement.Expression = expression;
            }
            AnalyzeTypes(modulePart, currentMethod.ReturnTypeNode, type);
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
                    throw new IllegalAssignmentException(modulePart.Source, dest.Line, dest.Column);
            }
            AnalyzeTypes(modulePart, destType, srcType);
        }

        private void AnalyzeExpressionStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionStatement expressionStatement)
        {
            var expression = expressionStatement.Expression;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref expression);
            expressionStatement.Expression = expression;
        }

        private void AnalyzeVariableDeclarationStatement(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
        {
            if (
                localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _) || 
                currentMethod?.Parameters.FirstOrDefault(p => p.Name == variableDeclarationStatement.Variable.Name) != null
            )
            {
                throw new RedefinedException(modulePart.Source, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
            var type = GetTypeFromTypeNode(modulePart, variableDeclarationStatement.Variable.TypeNode);
            variableDeclarationStatement.Variable.TypeNode = CreateTypeNodeFromType(
                type, 
                variableDeclarationStatement.Variable.TypeNode.Line, 
                variableDeclarationStatement.Variable.TypeNode.Column
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
                throw new RedefinedException(modulePart.Source, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
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
                throw new TypeDiscrepancyException(modulePart.Source, leftNode, rightNode, leftNode.Line, leftNode.Column);
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
                case NewObjectExpression newObjectExpression: return AnalyzeNewObjectExpression(modulePart, c, localSymbolTable, newObjectExpression);
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
            duplicateExpression.ReturnTypeNode = CreateTypeNodeFromType(returnType, duplicateExpression.Line, duplicateExpression.Column);
            return duplicateExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeThisExpression(ModulePart modulePart, ThisExpression thisExpression)
        {
            var returnType = GetTypeFromTypeNode(modulePart, thisExpression.ReturnTypeNode!);
            thisExpression.ReturnTypeNode = CreateTypeNodeFromType(returnType, thisExpression.Line, thisExpression.Column);
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
                        modulePart.Source,
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
                        modulePart.Source,
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
                        standardFieldAccessmentExpression.Line,
                        standardFieldAccessmentExpression.Column
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

        private TypeNode AnalyzeNewObjectExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, NewObjectExpression newObjectExpression)
        {
            foreach(var statement in newObjectExpression.FieldInitializations)
            {
                AnalyzeAssignmentStatement(modulePart, c, localSymbolTable, statement);
            }
            var returnType = GetTypeFromTypeNode(modulePart, newObjectExpression.ReturnTypeNode!);
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
                    modulePart.Source, 
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

        private TypeNode AnalyzeArrayIndexingExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ArrayIndexingExpression arrayIndexingExpression)
        {
            var arrayNode = arrayIndexingExpression.Array;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref arrayNode);
            arrayIndexingExpression.Array = arrayNode;
            var arrayType = GetTypeFromTypeNode(modulePart, arrayNode.ReturnTypeNode!);
            if (!arrayType.IsArray)
            {
                throw new IndexingOnNonArrayException(modulePart.Source, arrayNode.ReturnTypeNode!.FullName, arrayIndexingExpression.Line, arrayIndexingExpression.Column);
            }
            var indexNode = arrayIndexingExpression.Index;
            AnalyzeExpression(modulePart, c, localSymbolTable, ref indexNode);
            arrayIndexingExpression.Index = indexNode;
            if (indexNode.ReturnTypeNode!.CLRFullName != PrimitiveType.Int32 && indexNode.ReturnTypeNode.CLRFullName != PrimitiveType.Int64)
            {
                throw new InvalidArrayIndexingTypeException(modulePart.Source, indexNode.ReturnTypeNode.FullName, indexNode.Line, indexNode.Column);
            }
            arrayIndexingExpression.ReturnTypeNode = CreateTypeNodeFromType(arrayType.GetElementType()!, arrayIndexingExpression.Line, arrayIndexingExpression.Column);
            return arrayIndexingExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeNewArrayExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref NewArrayExpression newArrayExpression)
        {
            int? size = null;
            Type type = GetTypeFromTypeNode(modulePart, newArrayExpression.ReturnTypeNode!);
            newArrayExpression.ReturnTypeNode = CreateTypeNodeFromType(type, newArrayExpression.ReturnTypeNode!.Line, newArrayExpression.ReturnTypeNode.Column);

            // Analyze size expression if not null
            if (newArrayExpression.Size != null)
            {
                ExpressionNode sizeNode = newArrayExpression.Size;
                var sizeType = AnalyzeExpression(modulePart, c, localSymbolTable, ref sizeNode);
                newArrayExpression.Size = sizeNode;
                if (sizeType.CLRFullName != PrimitiveType.Int32 && sizeType.CLRFullName != PrimitiveType.Int64)
                {
                    throw new InvalidArraySizeTypeException(modulePart.Source, sizeType.FullName, sizeNode.Line, sizeNode.Column);
                }
                size = GetValueIfLiteral(sizeNode);
                if (size != null && size < 1)
                {
                    throw new NegativeArraySizeException(modulePart.Source, size.Value, sizeNode.Line, sizeNode.Line);
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
                        Line = newArrayExpression.Elements[i].Line,
                        Column = newArrayExpression.Elements[i].Column
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
                throw new NoSizeArrayWithoutInitializationException(modulePart.Source, newArrayExpression.ReturnTypeNode!.Line, newArrayExpression.ReturnTypeNode!.Column);
            }
            // Check if size is specified but number of elements is unequal
            if (size != null && size != newArrayExpression.Elements.Length && newArrayExpression.Elements.Length != 0)
            {
                throw new ArraySizeDiscrepancyException(
                    modulePart.Source, 
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
                        Line = methodCallExpression.Arguments[i].Line,
                        Column = methodCallExpression.Arguments[i].Column
                    };
                }
            }
        }

        private TypeNode AnalyzeStandardMethodCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref StandardMethodCallExpression standardMethodCallExpression)
        {
            if (currentMethod == null)
            {
                throw new MethodCallFromOutsideException(modulePart.Source, standardMethodCallExpression.Line, standardMethodCallExpression.Column);
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
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
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
                standardMethodCallExpression.Line,
                standardMethodCallExpression.Column
            );
            return standardMethodCallExpression.ReturnTypeNode;
        }

        private TypeNode AnalyzeStaticMethodCallExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            staticMethodCallExpression.TypeNode.CLRType = GetTypeFromTypeNode(modulePart, staticMethodCallExpression.TypeNode);
            var methodFullName = string.Join(".", [staticMethodCallExpression.TypeNode.CLRFullName, staticMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(modulePart, c, localSymbolTable, ref staticMethodCallExpression, methodFullName, staticMethodCallExpression.TypeNode.CLRType);
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
                throw new MethodCallFromOutsideException(modulePart.Source, ambiguousMethodCallExpression.Line, ambiguousMethodCallExpression.Column);
            }
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.MethodName]);
            AnalyzeMethodCallExpression(modulePart, c, localSymbolTable, ref ambiguousMethodCallExpression, methodFullName);
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
                        modulePart.Source, 
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
                            CreateTypeNodeFromType(typeTable.GetValueOrDefault(c.Name)!, methodCallExpression.Line, methodCallExpression.Column),
                        CreateTypeNodeFromType(type!, methodCallExpression.Line, methodCallExpression.Column)
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
                        modulePart.Source,
                        methodCallExpression.MethodName,
                        [..argumentTypes],
                        methodCallExpression.Line,
                        methodCallExpression.Column
                    );

                    if (!methodCallExpression.MethodInfo.IsPublic) throw new InvalidMethodCallException(
                        modulePart.Source,
                        methodCallExpression.MethodName,
                        [.. argumentTypes],
                        methodCallExpression.Line,
                        methodCallExpression.Column
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
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int64) ?? throw new("Internal error"), 
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
                    CLRType = module.CoreAssembly!.GetType(PrimitiveType.Int32) ?? throw new("Internal error"), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column 
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
        private Types.TypeNode AnalyzeVariableExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, ref ExpressionNode expression)
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
                throw new UnresolvedIdentifierException(modulePart.Source, variableExpression.Name, variableExpression.Line, variableExpression.Column);
            }
        }

        private Types.TypeNode AnalyzeBinaryExpression(ModulePart modulePart, ClassNode c, Dictionary<string, (TypeNode, LocalBuilder)> localSymbolTable, BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(modulePart, c, localSymbolTable,  ref left);
            var rightType = AnalyzeExpression(modulePart, c, localSymbolTable,  ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = leftType.CLRFullName;
            var rightTypeFullName = rightType.CLRFullName;
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(modulePart.Source, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            
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
                    Line = binaryExpression.Line,
                    Column = binaryExpression.Column
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
                            throw new AmbiguousTypeUsage(modulePart.Source, tn, ftn, stn, typeNode.Line, typeNode.Column);
                        }
                        if (tempType == null && type != null) continue;
                        type = tempType;
                    }
                    if (type == null)
                    {
                        throw new UnresolvedTypeException(modulePart.Source, typeNode.FullName, typeNode.Line, typeNode.Column);
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
