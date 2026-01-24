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
        private readonly Dictionary<string, MethodInfo> moduleMethods = [];
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
            for (int i = 0; i < module.Classes.Length; i++)
            {
                AnalyzeClass(module.Classes[i]);
            }
        }

        private void AnalyzeClass(ClassNode c)
        {
            TypeBuilder typeBuilder = module.ModuleBuilder!.DefineType(
                c.Name,
                TypeAttributes.Public | TypeAttributes.Class
            );
            c.TypeBuilder = typeBuilder;
            if (!typeTable.TryAdd(c.Name, c.TypeBuilder))
            {
                throw new RedefinedException(content, c.Name, c.Line, c.Column);
            }
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(c, typeBuilder, c.Methods[i]);
            }
        }

        private void AnalyzeMethod(ClassNode c, TypeBuilder typeBuilder, MethodNode method)
        {
            SymbolTable<TypeNode> localSymbolTable = new();
            try
            {
                method.ReturnType.Type = module.CoreAssembly!.GetType(method.ReturnType.Name) ?? throw new();
            }
            catch (Exception)
            {
                throw new UnresolvedTypeException(content, method.ReturnType.Name, method.ReturnType.Line, method.ReturnType.Column);
            }
            foreach (var parameter in method.Parameters)
            {
                try
                {
                    parameter.Type.Type = module.CoreAssembly!.GetType(parameter.Type.Name) ?? throw new();
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
                    throw new UnresolvedTypeException(content, parameter.Type.Name, parameter.Type.Line, parameter.Type.Column);
                }
            }
            method.MethodBuilder = typeBuilder.DefineMethod(
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
            if (!moduleMethods.TryAdd(methodFullName, method.MethodBuilder))
            {
                throw new RedefinedException(content, methodFullName, method.Line, method.Column);
            }
            for (int i = 0; i < method.Statements.Length; i++)
            {
                AnalyzeStatement(localSymbolTable,  method.Statements[i]);
            }
        }

        private void AnalyzeStatement(SymbolTable<Types.TypeNode> localSymbolTable,  Types.StatementNode statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(localSymbolTable,  variableDeclarationStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeVariableDeclarationStatement(SymbolTable<Types.TypeNode> localSymbolTable,  VariableDeclarationStatement variableDeclarationStatement)
        {
            if (localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
            try
            {
                variableDeclarationStatement.Variable.Type.Type = module.CoreAssembly!.GetType(variableDeclarationStatement.Variable.Type.Name) ?? throw new();
            }
            catch (Exception)
            {
                throw new UnresolvedTypeException(
                    content, 
                    variableDeclarationStatement.Variable.Type.Name, 
                    variableDeclarationStatement.Variable.Type.Line, 
                    variableDeclarationStatement.Variable.Type.Column
                );
            }
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(localSymbolTable,  expr);
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

        private Types.TypeNode AnalyzeExpression(SymbolTable<Types.TypeNode> localSymbolTable,  ExpressionNode expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(literalExpression);
                case VariableExpression variableExpression: return AnalyzeVariableExpression(localSymbolTable, variableExpression);
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(localSymbolTable,  binaryExpression);
                default: throw new NotImplementedException();
            }
        }

        private Types.TypeNode AnalyzeLiteralExpression(LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                literalExpression.ReturnType = new Types.TypeNode { Name = PrimitiveType.String, Type = module.CoreAssembly!.GetType(PrimitiveType.String), Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                literalExpression.ReturnType = new Types.TypeNode { Name = PrimitiveType.Int64, Type = module.CoreAssembly!.GetType(PrimitiveType.Int64), Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int64.TryParse(literalExpression.Value, out _))
            {
                literalExpression.ReturnType = new Types.TypeNode { Name = PrimitiveType.Int32, Type = module.CoreAssembly!.GetType(PrimitiveType.Int32) , Line = literalExpression.Line, Column = literalExpression.Column };
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

        private Types.TypeNode AnalyzeBinaryExpression(SymbolTable<Types.TypeNode> localSymbolTable,  BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(localSymbolTable,  left);
            var rightType = AnalyzeExpression(localSymbolTable,  right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            if (leftType.Name != rightType.Name) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.ReturnType = rightType;
            return rightType;
        }
    }
}
