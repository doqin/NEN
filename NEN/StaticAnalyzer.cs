using NEN.Exceptions;
using NEN.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    public class StaticAnalyzer(string[] contentLines)
    {
        private readonly string[] content = contentLines;

        public void Analyze(ref Types.Module module)
        {
            for (int i = 0; i < module.Classes.Length; i++)
            {
                AnalyzeClass(ref module.Classes[i]);
            }
        }

        private void AnalyzeClass(ref Types.Class c)
        {
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(ref c.Methods[i]);
            }
        }

        private void AnalyzeMethod(ref Types.Method method)
        {
            SymbolTable<Types.Type> localSymbolTable = new();
            for (int i = 0; i < method.Statements.Length; i++)
            {
                AnalyzeStatement(ref localSymbolTable, ref method.Statements[i]);
            }
        }

        private void AnalyzeStatement(ref SymbolTable<Types.Type> localSymbolTable, ref Types.Statement statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(ref localSymbolTable, ref variableDeclarationStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeVariableDeclarationStatement(ref SymbolTable<Types.Type> localSymbolTable, ref VariableDeclarationStatement variableDeclarationStatement)
        {
            if (localSymbolTable.TryGetValue(variableDeclarationStatement.Variable.Name, out _))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
            if (variableDeclarationStatement.InitialValue == null) { }
            else
            {
                var expr = variableDeclarationStatement.InitialValue;
                AnalyzeExpression(ref localSymbolTable, ref expr);
                if (expr.Type!.Name != variableDeclarationStatement.Variable.Type.Name)
                {
                    throw new TypeDiscrepancyException(content, variableDeclarationStatement.Variable.Type, expr.Type, variableDeclarationStatement.Line, variableDeclarationStatement.Column);
                }
                variableDeclarationStatement.InitialValue = expr;
            }
            if (!localSymbolTable.TryAdd(variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Type))
            {
                throw new RedefinedException(content, variableDeclarationStatement.Variable.Name, variableDeclarationStatement.Variable.Line, variableDeclarationStatement.Variable.Column);
            }
        }

        private Types.Type AnalyzeExpression(ref SymbolTable<Types.Type> localSymbolTable, ref Expression expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(ref literalExpression);
                case VariableExpression variableExpression: return AnalyzeVariableExpression(ref localSymbolTable, ref variableExpression);
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(ref localSymbolTable, ref binaryExpression);
                default: throw new NotImplementedException();
            }
        }

        private Types.Type AnalyzeLiteralExpression(ref LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                literalExpression.Type = new Types.Type { Name = PrimitiveType.String, Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else if (literalExpression.Value.EndsWith('L'))
            {
                literalExpression.Type = new Types.Type { Name = PrimitiveType.Int64, Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[0..^1];
            }
            else if (Int64.TryParse(literalExpression.Value, out _))
            {
                literalExpression.Type = new Types.Type { Name = PrimitiveType.Int32, Line = literalExpression.Line, Column = literalExpression.Column };
            }
            else
            {
                throw new NotImplementedException();
            }
            return literalExpression.Type;
        }

        private Types.Type AnalyzeVariableExpression(ref SymbolTable<Types.Type> localSymbolTable, ref VariableExpression variableExpression)
        {
            if (localSymbolTable.TryGetValue(variableExpression.Name, out var type))
            {
                variableExpression.Type = type;
                return type!;
            }
            else
            {
                throw new UnresolvedIdentifierException(content, variableExpression.Name, variableExpression.Line, variableExpression.Column);
            }
        }

        private Types.Type AnalyzeBinaryExpression(ref SymbolTable<Types.Type> localSymbolTable, ref BinaryExpression binaryExpression)
        {
            var left = binaryExpression.Left;
            var right = binaryExpression.Right;

            var leftType = AnalyzeExpression(ref localSymbolTable, ref left);
            var rightType = AnalyzeExpression(ref localSymbolTable, ref right);

            binaryExpression.Left = left;
            binaryExpression.Right = right;

            if (leftType.Name != rightType.Name) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.Type = rightType;
            return rightType;
        }
    }
}
