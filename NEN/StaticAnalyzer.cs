using NEN.Types;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    public class StaticAnalyzer
    {
        public static void Analyze(ref Types.Module module)
        {
            for (int i = 0; i < module.Classes.Length; i++)
            {
                AnalyzeClass(ref module.Classes[i]);
            }
        }

        private static void AnalyzeClass(ref Types.Class c)
        {
            for (int i = 0; i < c.Methods.Length; i++)
            {
                AnalyzeMethod(ref c.Methods[i]);
            }
        }

        private static void AnalyzeMethod(ref Types.Method method)
        {
            for (int i = 0; i < method.Statements.Length; i++)
            {
                AnalyzeStatement(ref method.Statements[i]);
            }
        }

        private static void AnalyzeStatement(ref Types.Statement statement)
        {
            switch(statement)
            {
                case VariableDeclarationStatement variableDeclarationStatement:
                    AnalyzeVariableDeclarationStatement(ref variableDeclarationStatement);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private static void AnalyzeVariableDeclarationStatement(ref VariableDeclarationStatement variableDeclarationStatement)
        {
            if (variableDeclarationStatement.InitialValue == null) return;
            var expr = variableDeclarationStatement.InitialValue;
            AnalyzeExpression(ref expr);
            variableDeclarationStatement.InitialValue = expr;
        }

        private static void AnalyzeExpression(ref Expression expression)
        {
            switch(expression)
            {
                case LiteralExpression literalExpression:
                    AnalyzeLiteralExpression(ref literalExpression);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        private static void AnalyzeLiteralExpression(ref LiteralExpression literalExpression)
        {
            if (literalExpression.Value.StartsWith('"') && literalExpression.Value.EndsWith('"'))
            {
                literalExpression.Type = new Types.Type { Name = Types.Type.String, Line = literalExpression.Line, Column = literalExpression.Column };
                literalExpression.Value = literalExpression.Value[1..^1];
            }
            else
            {
                throw new NotImplementedException();
            }
        }
    }
}
