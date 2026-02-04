using System.Reflection;
using System.Reflection.Emit;

namespace NEN
{
    namespace AST
    {
        public class ModulePart
        {
            public required string SourceName { get; set; }
            public required string[] Source { get; set; }
            public ClassNode[] Classes { get; set; } = [];
            public UsingNamespaceStatement[] UsingNamespaces { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString<ASTNode>($"Bộ phận mô đun: {SourceName}", [.. UsingNamespaces, .. Classes]);
            }
        }

        public class Module
        {
            public required string Name { get; set; }
            public required ModulePart[] ModuleParts { get; set; }
            public string[] AvailableNamespaces { get; set; } = [];
            public MetadataLoadContext? MetadataLoadContext { get; set; }
            public Assembly? CoreAssembly { get; set; }
            public PersistedAssemblyBuilder? AssemblyBuilder { get; set; }
            public ModuleBuilder? ModuleBuilder { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString($"Mô đun: {Name}", [.. ModuleParts]);
            }
        }
        public abstract class ASTNode
        {
            public required int StartLine { set; get; }
            public required int StartColumn { set; get; }
            public required int EndLine { set; get; }
            public required int EndColumn { set; get; }
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
                return Helper.GetTreeString<ASTNode>($"Lớp: {Name}{isResolved}", [.. Fields, .. Constructors ?? [], .. Methods]);
            }
        }

        public abstract class MethodBase : ASTNode
        {
            public MethodAttributes MethodAttributes { get; set; } = MethodAttributes.Private;
            public VariableNode[] Parameters { get; set; } = [];
            public StatementNode[] Statements { get; set; } = [];
            public abstract System.Reflection.MethodBase? GetMethodInfo();
        }

        public class MethodNode : MethodBase
        {
            public required TypeNode ReturnTypeNode { get; set; }
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
            public required NamedType DeclaringTypeNode { get; set; }
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
                    $"Phương thức khởi tạo: ({MethodAttributes}) {isResolved}({parameters})",
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
            public static readonly string Boolean = "System.Boolean";
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

        public class IfStatement : StatementNode
        {
            public required ExpressionNode Condition { get; set; }
            public required StatementNode[] IfClause { get; set; }
            public required StatementNode[] ElseClause { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString<object>(
                    $"Nếu:",
                    [
                        Condition,
                        Helper.GetTreeString("Phần nếu:",IfClause),
                        Helper.GetTreeString("Phần không thì:", ElseClause)
                        ]);
            }
        }

        public class WhileStatement : StatementNode
        {
            public required ExpressionNode Condition { get; set; }
            public required StatementNode[] Body { get; set; }
            public Label? EndLabel { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString<object>("Trong khi:", [Condition, Helper.GetTreeString("Nội dung:", [.. Body])]);
            }
        }

        public class BreakStatement : StatementNode
        {
            public Label? EndLabel { get; set; }
        }

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

        public abstract class DeclarationStatement : StatementNode
        {
            public required VariableNode Variable { set; get; }
            public ExpressionNode? InitialValue { set; get; }
        }

        public class VariableDeclarationStatement : DeclarationStatement
        {
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

        public class FieldDeclarationStatement : DeclarationStatement
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

        public abstract class ExpressionNode : ASTNode
        {
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

        public class ThisExpression : ExpressionNode
        {
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
                return $"{Value}(*)";
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
                return Helper.GetTreeString($"Gọi hàm: {TypeNode.FullName}::{MethodName}{isResolved}{returnType}", Arguments);
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
                return Helper.GetTreeString($"Gọi hàm: {namespaceAndType}::{MethodName}{isResolved}{returnType}", [Object, .. Arguments]);
            }
        }

        public class BoxExpression : ExpressionNode
        {
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

        public abstract class NewObjectExpression : ExpressionNode
        {
            public ConstructorInfo? ConstructorInfo { get; set; }
        }

        public class InlineConstructionExpression : NewObjectExpression // For constructing an object with inline field initialization
        {
            public required AssignmentStatement[] FieldInitializations { get; set; } // sometimes you gotta use statements inside an expression lol
            public override string ToString()
            {
                string isResolved = ConstructorInfo == null ? "(*)" : "";
                return Helper.GetTreeString($"Khởi tạo đối tượng kiểu {ReturnTypeNode}{isResolved}", FieldInitializations);
            }
        }

        public class ConstructorCallExpression : NewObjectExpression
        {
            public required ExpressionNode[] Arguments { get; set; }
            public override string ToString()
            {
                string isResolved = ConstructorInfo == null ? "(*)" : "";
                return Helper.GetTreeString($"Khởi tạo đối tượng kiểu {ReturnTypeNode}{isResolved}", Arguments);
            }
        }

        public abstract class FieldAccessmentExpression : ExpressionNode
        {
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

        public class DuplicateExpression : ExpressionNode
        {
            public override string ToString()
            {
                return "Sao chép đối tượng";
            }
        }
    }
}