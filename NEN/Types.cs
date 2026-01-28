using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;

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
            public ClassNode[] Classes { get; set; } = [];
            public MetadataLoadContext? MetadataLoadContext { get; set; }
            public Assembly? CoreAssembly { get; set; }
            public PersistedAssemblyBuilder? AssemblyBuilder { get; set; }
            public ModuleBuilder? ModuleBuilder { get; set; }
            public UsingNamespaceStatement[] UsingNamespaces { get; set; } = [];
            public string[] AvailableNamespaces { get; set; } = [];
            public override string ToString()
            {
                return Helper.GetTreeString<ASTNode>($"Mô đun: {Name}", [.. UsingNamespaces, .. Classes]);
            }
        }

        public abstract class ASTNode
        {
            public required int Line { set; get; }
            public required int Column { set; get; }
        }

        public class ClassNode : ASTNode
        {
            public required string Name { get; set; }
            public FieldDeclarationStatement[] Fields { get; set; } = [];
            public MethodNode[] Methods { get; set; } = [];
            public TypeBuilder? TypeBuilder { get; set; }
            public ConstructorNode[]? Constructors { get; set; }
            public ConstructorNode? GetDefaultConstructor()
            {
                return Constructors?.First(e => e.Parameters.Length == 0);
            }
            public override string ToString()
            {
                string isResolved = TypeBuilder == null ? "(*)" : "";
                return Helper.GetTreeString<ASTNode>($"Lớp: {Name}{isResolved}", [.. Fields, ..Constructors ?? [], .. Methods]);
            }
        }

        public abstract class MethodBase : ASTNode
        {
            public MethodAttributes MethodAttributes { get; set; } = MethodAttributes.Private;
            public VariableNode[] Parameters { get; set; } = [];
            public required TypeNode ReturnTypeNode { get; set; }
            public StatementNode[] Statements { get; set; } = [];
            public abstract System.Reflection.MethodBase? GetMethodInfo();
        }

        public class MethodNode : MethodBase
        {
            public bool IsEntryPoint { get; set; } = false;
            public required string MethodName { get; set; }
            public MethodBuilder? MethodBuilder { get; set; }
            public override System.Reflection.MethodBase? GetMethodInfo()
            {
                return MethodBuilder;
            }
            public override string ToString()
            {
                string isEntryPoint = IsEntryPoint ? "(Hàm chính) " : "";
                string isResolved = MethodBuilder == null ? "(*)" : "";
                string parameters = string.Join(", ", Parameters.Select(param => $"{param.Name} thuộc {param.TypeNode}"));
                return Helper.GetTreeString<ASTNode>(
                    $"Phương thức: {isEntryPoint}({MethodAttributes}) {MethodName}{isResolved}({parameters}) -> {ReturnTypeNode}", 
                    [.. Statements]
                    );
            }
        }

        public class ConstructorNode : MethodBase
        {
            public ConstructorBuilder? ConstructorBuilder { get; set; }
            public override System.Reflection.MethodBase? GetMethodInfo()
            {
                return ConstructorBuilder;
            }
            public override string ToString()
            {
                string isResolved = ConstructorBuilder == null ? "(*)" : "";
                string parameters = string.Join(", ", Parameters.Select(param => $"{param.Name} thuộc {param.TypeNode}"));
                return Helper.GetTreeString(
                    $"Phương thức tạo đối tượng: ({MethodAttributes}) {isResolved}({parameters}) -> {ReturnTypeNode}",
                    [.. Statements]
                    );
            }
        }

        public class VariableNode : ASTNode // Used for both attributes and parameters
        {
            public required string Name { get; set; }
            public required TypeNode TypeNode { get; set; }
            public override string ToString()
            {
                return $"{Name} ({TypeNode})";
            }
        }

        public static class PrimitiveType
        {
            public static readonly string Int32 = "System.Int32";
            public static readonly string Int64 = "System.Int64";
            public static readonly string String = "System.String";
        }

        public abstract class TypeNode : ASTNode
        {
            public required string[] Namespaces { get; set; }
            public required string Name { get; set; }
            public string FullName => string.Join("::", [.. Namespaces, Name]);
            public string CLRFullName => string.Join(".", [.. Namespaces, Name]);
        }

        public class NamedType : TypeNode
        {
            public Type? CLRType { get; set; }
            public override string ToString()
            {
                if (CLRType == null)
                {
                    return $"{string.Join("::", [.. Namespaces, Name])}(*)";
                }
                return CLRType.FullName ?? string.Join("::", [.. Namespaces, Name]);
            }
        }

        public class ArrayType : TypeNode
        {
            public required TypeNode ElementTypeNode { get; set; }
            // For now: always 1
            public int Rank { get; set; } = 1;

            // For now: always true (SZARRAY)
            public bool IsJagged => Rank == 1;

            public override string ToString()
            {
                if (Rank == 1)
                    return $"{ElementTypeNode}[]";

                return $"{ElementTypeNode}[{new string(',', Rank - 1)}]";
            }
        }

        public abstract class StatementNode : ASTNode { }

        public class ReturnStatement : StatementNode
        {
            public ExpressionNode? Expression { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString($"Trả về:", [Expression]);
            }
        }

        public class UsingNamespaceStatement : StatementNode
        {
            public required string[] Namespace;
            public bool IsResolved = false;
            public override string ToString()
            {
                string isResolved = IsResolved ? "" : "(*)";
                return $"Sử dụng không gian tên '{string.Join("::", Namespace)}'{isResolved}";
            }
        }

        public class VariableDeclarationStatement : StatementNode
        {
            public required VariableNode Variable { set; get; }
            public ExpressionNode? InitialValue { set; get; }
            public LocalBuilder? LocalBuilder { set; get; }
            public override string ToString()
            {
                if (InitialValue != null)
                {
                    return Helper.GetTreeString($"Khai báo biến: {Variable} gán", [InitialValue]);
                }
                return $"Khai báo biến: {Variable}";
            }
        }

        public class FieldDeclarationStatement : VariableDeclarationStatement
        {
            public FieldInfo? FieldInfo { set; get; }
            public FieldAttributes FieldAttributes { get; set; } = FieldAttributes.Private;
            public override string ToString()
            {
                if (InitialValue != null)
                {
                    return Helper.GetTreeString($"Khai báo thuộc tính: ({FieldAttributes}) {Variable} gán", [InitialValue]);
                }
                return $"Khai báo thuộc tính: {Variable}";
            }
        }

        public class AssignmentStatement : StatementNode
        {
            public required ExpressionNode Destination { get; set; }
            public required ExpressionNode Source { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString("Gán:", [Destination, Source]);
            }
        }

        public class ExpressionStatement : StatementNode
        {
            public required ExpressionNode Expression { get; set; }
            public override string ToString()
            {
                return $"Biểu thức: {Expression}";
            }
        }

        public abstract class ExpressionNode : ASTNode {
            public TypeNode? ReturnTypeNode { get; set; }
        }

        public class BinaryExpression : ExpressionNode
        {
            public required ExpressionNode Left { get; set; }
            public required string Operator { get; set; }
            public required ExpressionNode Right { get; set; }
            public override string ToString()
            {
                if (ReturnTypeNode != null)
                {
                    var str = Helper.GetTreeString<object>(null, [Left, Operator, Right]);
                    var lines = str.Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);
                    lines[0] = $"{lines[0]} -> {ReturnTypeNode}";
                    return string.Join("\n", lines);
                }
                return Helper.GetTreeString<object>(null, [Left, Operator, Right]);
            }
        }

        public class ThisExpression : ExpressionNode {
            public override string ToString()
            {
                return "{này}";
            }
        }

        public class LiteralExpression : ExpressionNode
        {
            public required string Value { get; set; }
            public override string ToString()
            {
                if (ReturnTypeNode != null)
                {
                    return $"{Value} ({ReturnTypeNode})";
                }
                return Value;
            }
        }

        public class VariableExpression : ExpressionNode
        {
            public required string Name { get; set; }
            public bool IsLoading { get; set; } = true;
            public LocalBuilder? LocalBuilder { get; set; }
            public override string ToString()
            {
                var isResolved = LocalBuilder == null ? "(*)" : "";
                if (ReturnTypeNode != null)
                {
                    return $"{Name}{isResolved} ({ReturnTypeNode})";
                }
                return $"{Name}{isResolved}";
            }
        }

        public class ArgumentExpression : ExpressionNode
        {
            public required string Name { get; set; }
            public bool IsLoading { get; set; } = true;
            public int? Index { get; set; }
            public override string ToString()
            {
                var isResolved = Index == null ? "(*)" : "";
                if (ReturnTypeNode != null)
                {
                    return $"{Name}{isResolved} ({ReturnTypeNode})";
                }
                return $"{Name}{isResolved}";
            }
        }

        public class AmbiguousMethodCallExpression : ExpressionNode // For unresolved methods
        {
            public System.Reflection.MethodBase? MethodInfo { get; set; }
            public required string MethodName { get; set; }
            public required ExpressionNode[] Arguments { get; set; }
            public override string ToString()
            {
                string isResolved = MethodInfo == null ? "(*)" : "";
                string returnType = ReturnTypeNode == null ? "" : $" -> {ReturnTypeNode}";
                return Helper.GetTreeString($"Gọi hàm(*): {MethodName}{isResolved}{returnType}", Arguments);
            }
        }

        public class StaticMethodCallExpression : AmbiguousMethodCallExpression
        {
            public required NamedType TypeNode { get; set; }
            public override string ToString()
            {
                string isResolved = MethodInfo == null ? "(*)" : "";
                string returnType = ReturnTypeNode == null ? "" : $" -> {ReturnTypeNode}";
                return Helper.GetTreeString($"Gọi hàm: {TypeNode.FullName}{isResolved}{returnType}", Arguments);
            }
        }

        public class StandardMethodCallExpression : AmbiguousMethodCallExpression
        {
            public required ExpressionNode Object { get; set; }
            public override string ToString()
            {
                string isResolved = MethodInfo == null ? "(*)" : "";
                string namespaceAndType = Object.ReturnTypeNode == null ? "" : $"{Object.ReturnTypeNode?.FullName}";
                string returnType = ReturnTypeNode == null ? "" : $" -> {ReturnTypeNode}";
                return Helper.GetTreeString($"Gọi hàm: {namespaceAndType}{isResolved}{returnType}", [Object, .. Arguments]);
            }
        }

        public class BoxExpression : ExpressionNode {
            public required ExpressionNode Expression { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString("Box biểu thức", [Expression]);
            }
        }

        public class NewArrayExpression : ExpressionNode
        {
            public ExpressionNode? Size { get; set; }
            public required ExpressionNode[] Elements { get; set; }
            public override string ToString()
            {
                string size = Size == null ? "tự động" : Size.ToString()!;
                return Helper.GetTreeString($"Khởi tạo mảng {ReturnTypeNode} ({size})", Elements);
            }
        }

        public class ArrayIndexingExpression : ExpressionNode
        {
            public required ExpressionNode Array { get; set; }
            public required ExpressionNode Index { get; set; }
            public bool IsLoading { get; set; } = true;
            public override string ToString()
            {
                return Helper.GetTreeString($"Truy cập phần tử -> {ReturnTypeNode}", [Index]);
            }
        }


        public class NewObjectExpression : ExpressionNode
        {
            public required AssignmentStatement[] FieldInitializations { get; set; } // sometimes you gotta use statements inside an expression lol
            public ConstructorInfo? ConstructorInfo { get; set; }
            public override string ToString()
            {
                string isResolved = ConstructorInfo == null ? "(*)" : "";
                return Helper.GetTreeString($"Khởi tạo đối tượng kiểu {ReturnTypeNode}{isResolved}", FieldInitializations);
            }
        }

        public abstract class FieldAccessmentExpression : ExpressionNode {
            public FieldInfo? FieldInfo { get; set; }
            public required string FieldName { get; set; }
            public bool IsLoading { get; set; } = true;
        }

        public class StandardFieldAccessmentExpression : FieldAccessmentExpression
        {
            public required ExpressionNode Object { get; set; }
            public override string ToString()
            {
                string isResolved = FieldInfo == null ? "(*)" : "";
                return Helper.GetTreeString($"Truy cập thuộc tính {FieldName}{isResolved}", [Object]);
            }
        }

        public class StaticFieldAccessmentExpression : FieldAccessmentExpression
        {
            public required TypeNode TypeNode { get; set; }
            public override string ToString()
            {
                string isResolved = FieldInfo == null ? "(*)" : "";
                return $"Truy cập thuộc tính {TypeNode}::{FieldName}{isResolved}";
            }
        }

        public class DuplicateExpression : ExpressionNode {
            public override string ToString()
            {
                return "Sao chép đối tượng";
            }
        }
    }
}
