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

        internal class SymbolTable<V>
        {
            private readonly Dictionary<string, (V builder, int index)> _dict = [];
            private int _nextIndex = 0;

            public bool TryAdd(string key, V value)
            {
                if (_dict.TryAdd(key, (value, _nextIndex)))
                {
                    _nextIndex++;
                    return true;
                }
                return false;
            }

            public bool TryGetValue(string key, out V? value)
            {
                if (_dict.TryGetValue(key, out var entry))
                {
                    value = entry.builder;
                    return true;
                }
                value = default;
                return false;
            }

            public bool TryGetIndex(string key, out int index)
            {
                if (_dict.TryGetValue(key, out var entry))
                {
                    index = entry.index;
                    return true;
                }
                index = -1;
                return false;
            }
        }

        public class Module
        {
            public required string Name { get; set; }
            public ClassNode[] Classes { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Mô đun: {Name}", Classes);
            }
        }

        public abstract class ASTNode
        {
            public required int Line {  set; get; }
            public required int Column { set; get; }
        }

        public class ClassNode : ASTNode
        {
            public required string Name { get; set; }
            public MethodNode[] Methods { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Lớp: {Name}", Methods);
            }
        }

        public class MethodNode : ASTNode
        {
            public bool IsEntryPoint { get; set; } = false;
            public MethodAttributes Attributes { get; set; } = MethodAttributes.Public;
            public required string Name { get; set; }
            public VariableNode[] Parameters { get; set; } = [];
            public StatementNode[] Statements { get; set; } = [];
            public required TypeNode ReturnType { get; set; }

            public override string ToString()
            {
                string isEntryPoint = IsEntryPoint ? "(Hàm chính) " : "";
                return Helper.GetTreeString<ASTNode>($"Phương thức: {isEntryPoint}({Attributes.ToString()}) {Name} -> {ReturnType}", [.. Parameters, .. Statements]);
            }
        }

        public class VariableNode : ASTNode // Used for both attributes and parameters
        {
            public required string Name { get; set; }
            public required TypeNode Type { get; set; }
            public override string ToString()
            {
                return $"{Name} ({Type})";
            }
        }

        public static class PrimitiveType
        {
            public static readonly string Int32 = "System.Int32";
            public static readonly string Int64 = "System.Int64";
            public static readonly string String = "System.String";
        }

        public class TypeNode : ASTNode
        {
            public required string Name { get; set; }
            public override string ToString()
            {
                return Name;
            }
        }

        public abstract class StatementNode : ASTNode { }

        public class VariableDeclarationStatement : StatementNode
        {
            public required VariableNode Variable { set; get; }
            public ExpressionNode? InitialValue { set; get; }
            public override string ToString()
            {
                if (InitialValue != null)
                {
                    return Helper.GetTreeString($"Khai báo biến: {Variable} gán", [InitialValue]);
                }
                return $"Khai báo biến: {Variable}";
            }
        }

        public abstract class ExpressionNode : ASTNode { 
            public TypeNode? Type { get; set; }
        }

        public class BinaryExpression : ExpressionNode
        {
            public required ExpressionNode Left { get; set; }
            public required string Operator { get; set; }
            public required ExpressionNode Right { get; set; }
            public override string ToString()
            {
                if (Type != null)
                {
                    var str = Helper.GetTreeString<object>(null, [Left, Operator, Right]);
                    var lines = str.Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);
                    lines[0] = $"{lines[0]} -> {Type}";
                    return string.Join("\n", lines);
                }
                return Helper.GetTreeString<object>(null, [Left, Operator, Right]);
            }
        }

        public class LiteralExpression : ExpressionNode
        {
            public required string Value { get; set; }
            public override string ToString()
            {
                if (Type != null)
                {
                    return $"{Value} ({Type})";
                }
                return Value;
            }
        }

        public class VariableExpression : ExpressionNode
        {
            public required string Name { get; set; }
            public override string ToString()
            {
                if (Type != null)
                {
                    return $"{Name} ({Type})";
                }
                return Name;
            }
        }
    }
}
