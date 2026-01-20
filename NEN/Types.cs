using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NEN
{
    namespace Types
    {
        internal enum TokenType
        {
            Identifier, // variables
            Keyword, // reserved words
            Literal, // numeric, logical, textual and reference literals (e.g. true, 6.02e23, "music")
            Operator, // symbols that operate on arguments and produce results. (e.g. +, <, =)
            Punctuator, // punctuation characters and paired delimiters. (e.g. }, (, ;)
            Comment,
            Unknown // for further analysis
        };

        internal class Token
        {
            public required TokenType Type { get; set; }
            public required string Value { get; set; }
            public required int Line { get; set; }
            public required int Column { get; set; }
        }

        internal class Module
        {
            public required string Name { get; set; }
            public Class[] Classes { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Mô đun: {Name}", Classes);
            }
        }

        internal abstract class AST
        {
            public required string Name { get; set; }
            public required int Line {  set; get; }
            public required int Column { set; get; }
        }

        internal class Class : AST
        {
            public Method[] Methods { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString($"Lớp: {Name}", Methods);
            }
        }

        internal class Method : AST
        {
            public Variable[] Parameters { get; set; } = [];
            public Statement[] Statements { get; set; } = [];
            public required string ReturnType { get; set; }

            public override string ToString()
            {
                return Helper.GetTreeString<AST>($"Phương thức: {Name} -> {ReturnType}", [.. Parameters, .. Statements]);
            }
        }

        internal class Variable : AST // Used for both attributes and parameters
        {
            public required string Type { get; set; }
        }

        internal abstract class Statement : AST { }

        internal abstract class Expression : AST { }
    }
}
