using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    namespace Types
    {
        [Flags]
        public enum TokenType
        {
            Identifier = 1 << 0, // variables
            Keyword = 1 << 1, // reserved words
            Literal = 1 << 2, // numeric, logical, textual and reference literals (e.g. true, 6.02e23, "music")
            Operator = 1 << 3, // symbols that operate on arguments and produce results. (e.g. +, <, =)
            Punctuator = 1 << 4, // punctuation characters and paired delimiters. (e.g. }, (, ;)
            Comment = 1 << 5,
            Marker = 1 << 6,
            Unknown = 0 // for further analysis
        };

        public class Operator
        {
            public static readonly string Plus = "+";
            public static readonly string Minus = "-";
            public static readonly string Multiply = "*";
            public static readonly string Divide = "/";
        }

        public class Token
        {
            public required TokenType Type { get; set; }
            public required string Value { get; set; }
            public required int Line { get; set; }
            public required int Column { get; set; }
        }

        public class Module
        {
            public required string Name { get; set; }
            public Class[] Classes { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Mô đun: {Name}", Classes);
            }
        }

        public abstract class AST
        {
            public required int Line {  set; get; }
            public required int Column { set; get; }
        }

        public class Class : AST
        {
            public required string Name { get; set; }
            public Method[] Methods { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Lớp: {Name}", Methods);
            }
        }

        public class Method : AST
        {
            public bool IsEntryPoint { get; set; } = false;
            public MethodAttributes Attributes { get; set; } = MethodAttributes.Public;
            public required string Name { get; set; }
            public Variable[] Parameters { get; set; } = [];
            public Statement[] Statements { get; set; } = [];
            public required Type ReturnType { get; set; }

            public override string ToString()
            {
                string isEntryPoint = IsEntryPoint ? "(Hàm chính)" : "";
                return Helper.GetTreeString<AST>($"Phương thức: {isEntryPoint} ({Attributes.ToString()}) {Name} -> {ReturnType}", [.. Parameters, .. Statements]);
            }
        }

        public class Variable : AST // Used for both attributes and parameters
        {
            public required string Name { get; set; }
            public required Type Type { get; set; }
            public override string ToString()
            {
                return $"{Name} ({Type})";
            }
        }

        public class Type : AST
        {
            public static string Int32 = "System.Int32";
            public static string String = "String";
            public required string Name { get; set; }
            public override string ToString()
            {
                return Name;
            }
        }

        public abstract class Statement : AST { }

        public class VariableDeclarationStatement : Statement
        {
            public required Variable Variable { set; get; }
            public Expression? InitialValue { set; get; }
            public override string ToString()
            {
                if (InitialValue != null)
                {
                    return Helper.GetTreeString($"Biến: {Variable} gán", [InitialValue]);
                }
                return $"Biến: {Variable}";
            }
        }

        public abstract class Expression : AST { 
            public Type? Type { get; set; }
        }

        public class BinaryExpression : Expression
        {
            public required Expression Left;
            public required string Operator;
            public required Expression Right;
            public override string ToString()
            {
                return Helper.GetTreeString<object>(null, [Left, Operator, Right]);
            }
        }

        public class LiteralExpression : Expression
        {
            public required string Value { get; set; }
            public override string ToString()
            {
                return Value;
            }
        }
    }
}
