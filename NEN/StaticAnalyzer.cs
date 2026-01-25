using NEN.Exceptions;
using NEN.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
using System.Threading.Tasks;

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
            method.ReturnType.Type = GetTypeFromName(
                method.ReturnType.NamespaceAndName,
                method.ReturnType.Line,
                method.ReturnType.Column
           );
            foreach (var parameter in method.Parameters)
            {
                parameter.Type.Type = GetTypeFromName(parameter.Type.NamespaceAndName, parameter.Type.Line, parameter.Type.Column);
                if (!localSymbolTable.TryAdd(parameter.Name, parameter.Type))
                {
                    throw new RedefinedException(content, parameter.Name, parameter.Line, parameter.Column);
                }
            }
            method.MethodBuilder = c.TypeBuilder!.DefineMethod(
                method.Name,
                method.Attributes,
                method.ReturnType.Type!,
                [.. method.Parameters.Select(param => param.Type.Type!)]
            );
            for (int i = 0; i < method.Parameters.Length; i++)
            {
                int index = method.MethodBuilder.IsStatic ? i : i + 1;
                ParameterBuilder p = method.MethodBuilder.DefineParameter(i + 1, ParameterAttributes.None, method.Parameters[i].Name);
            }
            string methodFullName = string.Join('.', [c.Name, method.Name]);
            if (!moduleMethods.TryAdd((methodFullName, [.. method.Parameters.Select(param => param.Type.Type!)]), method.MethodBuilder))
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
            variableDeclarationStatement.Variable.Type.Type = GetTypeFromName(
                variableDeclarationStatement.Variable.Type.NamespaceAndName, 
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

        private void AnalyzeTypes(TypeNode left, TypeNode right)
        {
            var sameName = right.Type!.FullName == right.Type.FullName;
            var isSubClass = right.Type.IsSubclassOf(left.Type!);
            var isAssignable = right.Type.IsAssignableTo(left.Type);
            if (!sameName && !isSubClass && !isAssignable)
            {
                throw new TypeDiscrepancyException(content, left, right, left.Line, left.Column);
            }
        }

        private Types.TypeNode AnalyzeExpression(ClassNode c, MethodNode method, SymbolTable<Types.TypeNode> localSymbolTable, ref ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression variableExpression: return AnalyzeVariableExpression(localSymbolTable, variableExpression);
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
        private TypeNode AnalyzeStandardMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref StandardMethodCallExpression standardMethodCallExpression)
        {
            var objec = standardMethodCallExpression.Object;
            var type = AnalyzeExpression(c, method, localSymbolTable, ref objec);
            standardMethodCallExpression.Object = objec;
            if (
                method.MethodBuilder!.IsStatic &&
                string.Join(".", [c.Name, standardMethodCallExpression.Name]) ==
                string.Join(".", [.. type.NamespaceAndName, standardMethodCallExpression.Name])
            )
            {
                throw new StaticIllegalAccessmentException(
                    content,
                    string.Join("::", [c.Name, standardMethodCallExpression.Name]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            List<Type> argumentTypes = [];
            for (int i = 0; i < standardMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode t = AnalyzeExpression(c, method, localSymbolTable, ref standardMethodCallExpression.Arguments[i]);
                argumentTypes.Add(t.Type!);
            }
            if (moduleMethods.TryGetValue(
                (string.Join(".", [.. type.NamespaceAndName, standardMethodCallExpression.Name]),
                [.. argumentTypes]),
                out var methodInfo
                )
            )
            {
                standardMethodCallExpression.Info = methodInfo;
            }
            else
            {
                standardMethodCallExpression.Info = type.Type!.GetMethod(
                    standardMethodCallExpression.Name,
                    [.. argumentTypes]
                ) ?? throw new UnresolvedIdentifierException(
                    content,
                    string.Join("::", [.. type.NamespaceAndName, standardMethodCallExpression.Name]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            AnalyzeArguments(ref standardMethodCallExpression);
            standardMethodCallExpression.ReturnType = new TypeNode
            {
                NamespaceAndName = standardMethodCallExpression.Info!.ReturnType.FullName!.Split("."),
                Type = standardMethodCallExpression.Info.ReturnType,
                Line = standardMethodCallExpression.Line,
                Column = standardMethodCallExpression.Column
            };
            return standardMethodCallExpression.ReturnType;
        }

        private void AnalyzeArguments<T>(ref T methodCallExpression) where T : AmbiguousMethodCallExpression
        {
            var parameters = methodCallExpression.Info!.GetParameters();
            var objectType = GetTypeFromName(["System", "Object"], methodCallExpression.Line, methodCallExpression.Column);
            for (int i = 0; i < parameters.Length; i++)
            {
                if (parameters[i].ParameterType == objectType && methodCallExpression.Arguments[i].ReturnType!.Type!.IsValueType)
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

        private TypeNode AnalyzeStaticMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            staticMethodCallExpression.Type.Type = GetTypeFromName(
                staticMethodCallExpression.Type.NamespaceAndName, 
                staticMethodCallExpression.Line, 
                staticMethodCallExpression.Column
            );
            List<Type> argumentTypes = [];
            for (int i = 0; i < staticMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, method, localSymbolTable, ref staticMethodCallExpression.Arguments[i]);
                argumentTypes.Add(typ.Type!);
            }
            if (moduleMethods.TryGetValue(
                (string.Join(".", [..staticMethodCallExpression.Type.NamespaceAndName, staticMethodCallExpression.Name]), 
               [..argumentTypes]),
                out var methodInfo
                )
            )
            {
                staticMethodCallExpression.Info = methodInfo;
            }
            else
            {
                staticMethodCallExpression.Info = staticMethodCallExpression.Type.Type!.GetMethod(
                    staticMethodCallExpression.Name,
                    [.. argumentTypes]
                ) ?? throw new UnresolvedIdentifierException(
                    content,
                    string.Join("::", [.. staticMethodCallExpression.Type.NamespaceAndName, staticMethodCallExpression.Name]),
                    staticMethodCallExpression.Line,
                    staticMethodCallExpression.Column
                );
            }
            AnalyzeArguments(ref staticMethodCallExpression);
            staticMethodCallExpression.ReturnType = new TypeNode
            {
                NamespaceAndName = staticMethodCallExpression.Info!.ReturnType.FullName!.Split("."),
                Type = staticMethodCallExpression.Info.ReturnType,
                Line = staticMethodCallExpression .Line,
                Column = staticMethodCallExpression.Column
            };
            return staticMethodCallExpression.ReturnType;
        }

        private TypeNode AnalyzeAmbiguousMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            List<Type> argumentTypes = [];
            for (int i = 0; i < ambiguousMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, method, localSymbolTable, ref ambiguousMethodCallExpression.Arguments[i]);
                argumentTypes.Add(typ.Type!);
            }
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.Name]);
            if (moduleMethods.TryGetValue(
                (methodFullName,
               [.. argumentTypes]),
                out var methodInfo
                )
            )
            {
                ambiguousMethodCallExpression.Info = methodInfo;
            }
            else
            {
                throw new UnresolvedIdentifierException(
                    content,
                    ambiguousMethodCallExpression.Name,
                    ambiguousMethodCallExpression.Line,
                    ambiguousMethodCallExpression.Column
                );
            }
            AnalyzeArguments(ref ambiguousMethodCallExpression);
            if (ambiguousMethodCallExpression.Info!.IsStatic)
            {
                ambiguousMethodCallExpression = new StaticMethodCallExpression { 
                    Arguments = ambiguousMethodCallExpression.Arguments, 
                    Type = new TypeNode { 
                        NamespaceAndName = ambiguousMethodCallExpression.Info.DeclaringType!.FullName!.Split("."),
                        Type = methodInfo.DeclaringType,
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    Name = ambiguousMethodCallExpression.Name,
                    ReturnType = new TypeNode {
                        NamespaceAndName = ambiguousMethodCallExpression.Info.ReturnType!.FullName!.Split("."),
                        Type = ambiguousMethodCallExpression.Info.ReturnType,
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    Info = ambiguousMethodCallExpression.Info,
                    Line = ambiguousMethodCallExpression.Line,
                    Column = ambiguousMethodCallExpression.Column
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
                        ReturnType = new TypeNode {
                            NamespaceAndName = [c.Name],
                            Type = typeTable.GetValueOrDefault(c.Name),
                            Line = ambiguousMethodCallExpression.Line,
                            Column = ambiguousMethodCallExpression.Column
                        },
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    Name = ambiguousMethodCallExpression.Name,
                    ReturnType = new TypeNode {
                        NamespaceAndName = ambiguousMethodCallExpression.Info.ReturnType!.FullName!.Split("."),
                        Type = ambiguousMethodCallExpression.Info.ReturnType,
                        Line = ambiguousMethodCallExpression.Line,
                        Column = ambiguousMethodCallExpression.Column
                    },
                    Info = ambiguousMethodCallExpression.Info,
                    Line = ambiguousMethodCallExpression.Line,
                    Column = ambiguousMethodCallExpression.Column
                };
            }
            return ambiguousMethodCallExpression.ReturnType!;
        }

        private TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                string[] namespaceAndName = PrimitiveType.String.Split(".");
                literalExpression.ReturnType = new TypeNode { 
                    NamespaceAndName = namespaceAndName, 
                    Type = GetTypeFromName(namespaceAndName, literalExpression.Line, literalExpression.Column), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column
                };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                string[] namespaceAndName = PrimitiveType.Int64.Split(".");
                literalExpression.ReturnType = new TypeNode { 
                    NamespaceAndName = namespaceAndName, 
                    Type = GetTypeFromName(namespaceAndName, literalExpression.Line, literalExpression.Column), 
                    Line = literalExpression.Line, 
                    Column = literalExpression.Column 
                };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int32.TryParse(literalExpression.Value, out _))
            {
                string[] namespaceAndName = PrimitiveType.Int32.Split(".");
                literalExpression.ReturnType = new TypeNode { 
                    NamespaceAndName = namespaceAndName, 
                    Type = GetTypeFromName(namespaceAndName, literalExpression.Line, literalExpression.Column), 
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

        private Types.TypeNode AnalyzeBinaryExpression(ClassNode c, MethodNode method, SymbolTable<Types.TypeNode> localSymbolTable,  BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(c, method, localSymbolTable,  ref left);
            var rightType = AnalyzeExpression(c, method, localSymbolTable,  ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = leftType.Type!.FullName;
            var rightTypeFullName = rightType.Type!.FullName;
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnType = rightType;
            return rightType;
        }

        /* Helper */

        private Type GetTypeFromName(string[] typeNamespaceAndName, int line, int column)
        {
            if (typeTable.TryGetValue(string.Join(".", typeNamespaceAndName), out var t))
            {
                return t;
            }
            string typeNamespace = string.Join(".", typeNamespaceAndName[0..^1]);
            string typeName = typeNamespaceAndName.Last();
            if (string.IsNullOrEmpty(typeName))
            {
                throw new ArgumentException("Internal error");
            }
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
                    string tn = string.Join("::", typeNamespaceAndName);
                    string ftn = string.Join("::", type.FullName!.Split("."));
                    string stn = string.Join("::", tempType.FullName!.Split("."));
                    throw new AmbiguousTypeUsage(content, tn, ftn, stn, line, column);
                }
                type = tempType;
            }
            if (type == null)
            {
                throw new UnresolvedTypeException(content, string.Join("::", typeNamespaceAndName), line, column);
            }
            if (!typeTable.TryAdd(string.Join(".", typeNamespaceAndName), type))
            {
                throw new("Internal error");
            }
            return type;
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
