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

        private void SetupAssembly()
        {
            string runtimePath = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
            PathAssemblyResolver resolver = new([.. Directory.GetFiles(runtimePath, "*.dll"), .. assemblyPaths]);
            module.MetadataLoadContext = new(resolver);
            module.CoreAssembly = module.MetadataLoadContext.CoreAssembly!;
            module.AssemblyBuilder = new(new AssemblyName(assemblyName), module.CoreAssembly!);
            module.ModuleBuilder = module.AssemblyBuilder.DefineDynamicModule(module.Name);
        }

        public void Analyze()
        {
            SetupAssembly();
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
            try
            {
                method.ReturnType.Type = module.CoreAssembly!.GetType(string.Join(".", method.ReturnType.NamespaceAndName)) ?? throw new();
            }
            catch (Exception)
            {
                throw new UnresolvedTypeException(content, string.Join("::", method.ReturnType.NamespaceAndName), method.ReturnType.Line, method.ReturnType.Column);
            }
            foreach (var parameter in method.Parameters)
            {
                try
                {
                    parameter.Type.Type = module.CoreAssembly!.GetType(string.Join(".", parameter.Type.NamespaceAndName)) ?? throw new();
                    if (!localSymbolTable.TryAdd(parameter.Name, parameter.Type))
                    {
                        throw new RedefinedException(content, parameter.Name, parameter.Line, parameter.Column);
                    }
                }
                catch (RedefinedException)
                {
                    throw;
                }
                catch (Exception)
                {
                    throw new UnresolvedTypeException(content, string.Join("::", parameter.Type.NamespaceAndName), parameter.Type.Line, parameter.Type.Column);
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
                default: throw new NotImplementedException();
            }
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
            try
            {
                variableDeclarationStatement.Variable.Type.Type = module.CoreAssembly!.GetType(string.Join(".", variableDeclarationStatement.Variable.Type.NamespaceAndName)) ?? throw new();
            }
            catch (Exception)
            {
                throw new UnresolvedTypeException(
                    content,
                    string.Join("::", variableDeclarationStatement.Variable.Type.NamespaceAndName), 
                    variableDeclarationStatement.Variable.Type.Line, 
                    variableDeclarationStatement.Variable.Type.Column
                );
            }
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(c, method, localSymbolTable,  ref expr);
                variableDeclarationStatement.InitialValue = expr;
                var sameName = expr.ReturnType!.Type!.FullName == variableDeclarationStatement.Variable.Type!.Type!.FullName;
                var isSubClass = expr.ReturnType!.Type!.IsSubclassOf(variableDeclarationStatement.Variable.Type!.Type!);
                var isAssignable = expr.ReturnType!.Type!.IsAssignableTo(variableDeclarationStatement.Variable.Type!.Type!);
                if (!sameName && !isSubClass && !isAssignable)
                {
                    throw new TypeDiscrepancyException(content, variableDeclarationStatement.Variable.Type, expr.ReturnType, variableDeclarationStatement.Line, variableDeclarationStatement.Column);
                }
            }
            if (!localSymbolTable.TryAdd(variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Type))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
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
            var objectType = AnalyzeExpression(c, method,localSymbolTable, ref objec);
            standardMethodCallExpression.Object = objec;
            if (
                method.MethodBuilder!.IsStatic &&
                string.Join(".", [c.Name, standardMethodCallExpression.Name]) == 
                string.Join(".", [.. objectType.NamespaceAndName, standardMethodCallExpression.Name])
            )
            {
                throw new StaticIllegalAccessmentException(
                    content,
                    string.Join("::", [c.Name, standardMethodCallExpression.Name]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            List<Type> arguments = [];
            for (int i = 0; i < standardMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode type = AnalyzeExpression(c, method, localSymbolTable, ref standardMethodCallExpression.Arguments[i]);
                arguments.Add(type.Type!);
            }
            if (moduleMethods.TryGetValue(
                (string.Join(".", [.. objectType.NamespaceAndName, standardMethodCallExpression.Name]), 
                [.. arguments]), 
                out var methodInfo
                )
            )
            {
                standardMethodCallExpression.Info = methodInfo;
            } 
            else
            {
                standardMethodCallExpression.Info = objectType.Type!.GetMethod(
                    standardMethodCallExpression.Name,
                    [.. arguments]
                ) ?? throw new UnresolvedIdentifierException(
                    content,
                    string.Join("::", [.. objectType.NamespaceAndName, standardMethodCallExpression.Name]),
                    standardMethodCallExpression.Line,
                    standardMethodCallExpression.Column
                );
            }
            standardMethodCallExpression.ReturnType = new TypeNode
            {
                NamespaceAndName = standardMethodCallExpression.Info.ReturnType.FullName!.Split("."),
                Type = standardMethodCallExpression.Info.ReturnType,
                Line = standardMethodCallExpression.Line,
                Column = standardMethodCallExpression.Column
            };
            return standardMethodCallExpression.ReturnType;
        }

        private TypeNode AnalyzeStaticMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            if (typeTable.TryGetValue(string.Join(".", staticMethodCallExpression.Type.NamespaceAndName), out var type))
            {
                staticMethodCallExpression.Type.Type = type;
            }
            else
            {
                staticMethodCallExpression.Type.Type = module.CoreAssembly!.GetType(
                    string.Join(".", staticMethodCallExpression.Type.NamespaceAndName)
                ) ?? throw new UnresolvedTypeException(
                    content, 
                    string.Join("::", staticMethodCallExpression.Type.NamespaceAndName), 
                    staticMethodCallExpression.Type.Line, 
                    staticMethodCallExpression.Type.Column
                );
            }
            List<Type> arguments = [];
            for (int i = 0; i < staticMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, method, localSymbolTable, ref staticMethodCallExpression.Arguments[i]);
                arguments.Add(typ.Type!);
            }
            if (moduleMethods.TryGetValue(
                (string.Join(".", [..staticMethodCallExpression.Type.NamespaceAndName, staticMethodCallExpression.Name]), 
               [..arguments]),
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
                    [.. arguments]
                ) ?? throw new UnresolvedIdentifierException(
                    content,
                    string.Join("::", [.. staticMethodCallExpression.Type.NamespaceAndName, staticMethodCallExpression.Name]),
                    staticMethodCallExpression.Line,
                    staticMethodCallExpression.Column
                );
            }
            staticMethodCallExpression.ReturnType = new TypeNode
            {
                NamespaceAndName = staticMethodCallExpression.Info.ReturnType.FullName!.Split("."),
                Type = staticMethodCallExpression.Info.ReturnType,
                Line = staticMethodCallExpression .Line,
                Column = staticMethodCallExpression.Column
            };
            return staticMethodCallExpression.ReturnType;
        }

        private TypeNode AnalyzeAmbiguousMethodCallExpression(ClassNode c, MethodNode method, SymbolTable<TypeNode> localSymbolTable, ref AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            List<Type> arguments = [];
            for (int i = 0; i < ambiguousMethodCallExpression.Arguments.Length; i++)
            {
                TypeNode typ = AnalyzeExpression(c, method, localSymbolTable, ref ambiguousMethodCallExpression.Arguments[i]);
                arguments.Add(typ.Type!);
            }
            var methodFullName = string.Join(".", [c.Name, ambiguousMethodCallExpression.Name]);
            if (moduleMethods.TryGetValue(
                (methodFullName,
               [.. arguments]),
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
            if (ambiguousMethodCallExpression.Info.IsStatic)
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

        private Types.TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                literalExpression.ReturnType = new Types.TypeNode { NamespaceAndName = ["System", "String"], Type = module.CoreAssembly!.GetType(PrimitiveType.String), Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                literalExpression.ReturnType = new Types.TypeNode { NamespaceAndName = ["System", "Int64"], Type = module.CoreAssembly!.GetType(PrimitiveType.Int64), Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int32.TryParse(literalExpression.Value, out _))
            {
                literalExpression.ReturnType = new Types.TypeNode { NamespaceAndName = ["System", "Int32"], Type = module.CoreAssembly!.GetType(PrimitiveType.Int32) , Line = literalExpression.Line, Column = literalExpression.Column };
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

            var leftTypeFullName = string.Join("::", leftType.NamespaceAndName);
            var rightTypeFullName = string.Join("::", rightType.NamespaceAndName);
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnType = rightType;
            return rightType;
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
