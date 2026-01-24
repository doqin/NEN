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
        private readonly Dictionary<(string, Type[]), MethodInfo> moduleMethods = [];
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
            List<SymbolTable<TypeNode>[]> lsLsSt = [];
            // Define every class and method in the module
            foreach (var c in module.Classes)
            {
                lsLsSt.Add(DefineClass(c));
            }
            // Analyze its the method bodies in each class
            for (int i = 0; i < module.Classes.Length; i++)
            {
                AnalyzeClass(module.Classes[i], lsLsSt[i]);
            }
        }

        private SymbolTable<TypeNode>[] DefineClass(ClassNode c)
        {
            c.TypeBuilder = module.ModuleBuilder!.DefineType(
                c.Name,
                TypeAttributes.Public | TypeAttributes.Class
            );
            if (!typeTable.TryAdd(c.Name, c.TypeBuilder))
            {
                throw new RedefinedException(content, c.Name, c.Line, c.Column);
            }
            List<SymbolTable<TypeNode>> lsSt = [];
            foreach (var method in c.Methods)
            {
                lsSt.Add(DefineMethod(c, method));
            }
            return [.. lsSt];
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
                AnalyzeMethod(c, localSymbolTableList[i], c.Methods[i]);
            }
        }

        private void AnalyzeMethod(ClassNode c, SymbolTable<TypeNode> localSymbolTable, MethodNode method)
        {
            foreach (var statement in method.Statements)
            {
                AnalyzeStatement(c, localSymbolTable, statement);
            }
        }

        private void AnalyzeStatement(ClassNode c, SymbolTable<TypeNode> localSymbolTable, StatementNode statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(c, localSymbolTable,  variableDeclarationStatement); break;
                case ExpressionStatement expressionStatement: AnalyzeExpressionStatement(c, localSymbolTable, expressionStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeExpressionStatement(ClassNode c, SymbolTable<TypeNode> localSymbolTable, ExpressionStatement expressionStatement)
        {
            AnalyzeExpression(c, localSymbolTable, expressionStatement.Expression);
        }

        private void AnalyzeVariableDeclarationStatement(ClassNode c, SymbolTable<Types.TypeNode> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
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
                AnalyzeExpression(c, localSymbolTable,  expr);
                var sameName = expr.ReturnType!.Type!.FullName == variableDeclarationStatement.Variable.Type!.Type!.FullName;
                var isSubClass = expr.ReturnType!.Type!.IsSubclassOf(variableDeclarationStatement.Variable.Type!.Type!);
                var isAssignable = expr.ReturnType!.Type!.IsAssignableTo(variableDeclarationStatement.Variable.Type!.Type!);
                if (!sameName && !isSubClass && !isAssignable)
                {
                    throw new TypeDiscrepancyException(content, variableDeclarationStatement.Variable.Type, expr.ReturnType, variableDeclarationStatement.Line, variableDeclarationStatement.Column);
                }
                variableDeclarationStatement.InitialValue = expr;
            }
            if (!localSymbolTable.TryAdd(variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Type))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
        }

        private Types.TypeNode AnalyzeExpression(ClassNode c, SymbolTable<Types.TypeNode> localSymbolTable,  ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression variableExpression: return AnalyzeVariableExpression(localSymbolTable, variableExpression);
                case StandardMethodCallExpression standardMethodCallExpression: return AnalyzeStandardMethodCallExpression(c, localSymbolTable, standardMethodCallExpression);
                case StaticMethodCallExpression staticMethodCallExpression: return AnalyzeStaticMethodCallExpression(c, localSymbolTable, staticMethodCallExpression);
                case AmbiguousMethodCallExpression ambiguousMethodCallExpression: return AnalyzeAmbiguousMethodCallExpression(c, localSymbolTable, ambiguousMethodCallExpression);
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(c, localSymbolTable,  binaryExpression);
                default: throw new NotImplementedException();
            }
        }
        private TypeNode AnalyzeStandardMethodCallExpression(ClassNode c, SymbolTable<TypeNode> localSymbolTable, StandardMethodCallExpression standardMethodCallExpression)
        {
            var objectType = AnalyzeExpression(c, localSymbolTable, standardMethodCallExpression.Object);
            List<Type> arguments = [];
            foreach(var arg in standardMethodCallExpression.Arguments)
            {
                TypeNode type = AnalyzeExpression(c, localSymbolTable, arg);
                arguments.Add(type.Type!);
            }
            if (moduleMethods.TryGetValue(
                (string.Join(".", objectType.NamespaceAndName), 
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

        private TypeNode AnalyzeStaticMethodCallExpression(ClassNode c, SymbolTable<TypeNode> localSymbolTable, StaticMethodCallExpression staticMethodCallExpression)
        {
            throw new NotImplementedException();
        }

        private TypeNode AnalyzeAmbiguousMethodCallExpression(ClassNode c, SymbolTable<TypeNode> localSymbolTable, AmbiguousMethodCallExpression ambiguousMethodCallExpression)
        {
            throw new NotImplementedException();
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

        private Types.TypeNode AnalyzeBinaryExpression(ClassNode c, SymbolTable<Types.TypeNode> localSymbolTable,  BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(c, localSymbolTable,  left);
            var rightType = AnalyzeExpression(c, localSymbolTable,  right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            var leftTypeFullName = string.Join("::", leftType.NamespaceAndName);
            var rightTypeFullName = string.Join("::", rightType.NamespaceAndName);
            if (leftTypeFullName != rightTypeFullName) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnType = rightType;
            return rightType;
        }
    }
}
