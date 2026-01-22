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
            for (int i = 0; i < method.Statements.Length; i++)
            {
                AnalyzeStatement(ref method.Statements[i]);
            }
        }

        private void AnalyzeStatement(ref Types.Statement statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement: AnalyzeVariableDeclarationStatement(ref variableDeclarationStatement); break;
                default: throw new NotImplementedException();
            }
        }

        private void AnalyzeVariableDeclarationStatement(ref VariableDeclarationStatement variableDeclarationStatement)
        {
            if (variableDeclarationStatement.InitialValue == null) return;
            var expr = variableDeclarationStatement.InitialValue;
            AnalyzeExpression(ref expr);
            variableDeclarationStatement.InitialValue = expr;
        }

        private Types.Type AnalyzeExpression(ref Expression expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression: return AnalyzeLiteralExpression(ref literalExpression);
                case BinaryExpression binaryExpression: return AnalyzeBinaryExpression(ref binaryExpression);
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

        private Types.Type AnalyzeBinaryExpression(ref BinaryExpression binaryExpression)
        {
            var leftType = AnalyzeExpression(ref binaryExpression.Left);
            var rightType = AnalyzeExpression(ref binaryExpression.Right);
            if (leftType.Name != rightType.Name) throw new TypeDiscrepancyException(content, leftType, rightType, binaryExpression.Line, binaryExpression.Column);
            binaryExpression.Type = rightType;
            return rightType;
        }
    }
}
