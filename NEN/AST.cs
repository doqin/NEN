using ICSharpCode.AvalonEdit.Document;
using Microsoft.VisualBasic;
using NEN.Symbols;
using System.Reflection;
using System.Reflection.Emit;

namespace NEN
{
    namespace AST
    {
        public class ModulePart
        {
            public required string SourceName { get; set; }
            public ClassNode[] Classes { get; set; } = [];
            public UsingNamespaceStatement[] UsingNamespaces { get; set; } = [];

            public override string ToString()
            {
                return Helper.GetTreeString<ASTNode>($"Bộ phận mô đun: {SourceName}", [.. UsingNamespaces, .. Classes]);
            }
            internal void CollectSymbols(
                TextDocument document,
                HashSet<Symbol> symbols,
                List<SymbolSpan> symbolSpans,
                bool collectPrivates,
                bool collectSpans)
            {
                foreach (var c in Classes)
                {
                    c.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
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
            public (Symbol[], SymbolSpan[]) CollectSymbols(string sourceFile, TextDocument document)
            {
                HashSet<Symbol> symbols = new(new SymbolComparer());
                List<SymbolSpan> symbolSpans = [];
                foreach (var modulePart in ModuleParts)
                {
                    if (modulePart.SourceName == sourceFile)
                    {
                        modulePart.CollectSymbols(document, symbols, symbolSpans, true, true);
                    }
                    else
                    {
                        modulePart.CollectSymbols(document, symbols, symbolSpans, false, false);
                    }
                }
                return ([.. symbols], [.. symbolSpans]);
            }
        }
        public abstract class ASTNode
        {
            public required int StartLine { set; get; }
            public required int StartColumn { set; get; }
            public required int EndLine { set; get; }
            public required int EndColumn { set; get; }
            internal abstract void CollectSymbols(
                TextDocument document,
                HashSet<Symbol> symbols,
                List<SymbolSpan> symbolSpans,
                bool collectPrivates,
                bool collectSpans
            );
        }

        public class ClassNode : ASTNode
        {
            public TypeNode? BaseTypeNode { get; set; } = null;
            public required string[] Namespaces { set; get; }
            public required string Name { get; set; }
            public FieldDeclarationStatement[] Fields { get; set; } = [];
            public MethodNode[] Methods { get; set; } = [];
            public TypeBuilder? TypeBuilder { get; set; }
            public ConstructorNode[]? Constructors { get; set; }
            public string FullName => string.Join("::", [.. Namespaces, Name]);
            public string CLRFullName => string.Join(".", [.. Namespaces, Name]);
            internal override void CollectSymbols(
                TextDocument document,
                HashSet<Symbol> symbols,
                List<SymbolSpan> symbolSpans,
                bool collectPrivates,
                bool collectSpans
            )
            {
                var classSymbol = new ClassSymbol { Name = Name };
                symbols.Add(classSymbol);
                BaseTypeNode?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach (var field in Fields)
                {
                    field.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                foreach (var constructor in Constructors ?? [])
                {
                    constructor.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                foreach (var method in Methods)
                {
                    method.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                if (collectSpans)
                {
                    Helper.AddSpan(
                        document,
                        symbolSpans,
                        StartLine,
                        StartColumn,
                        EndLine,
                        EndColumn,
                        SymbolKind.Class
                    ); // Class name
                }
            }

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
            public required NamedType DeclaringTypeNode { get; set; }
            public MethodAttributes MethodAttributes { get; set; } = MethodAttributes.Private;
            public VariableNode[] Parameters { get; set; } = [];
            public StatementNode[] Statements { get; set; } = [];
            public abstract System.Reflection.MethodBase? GetMethodInfo();
            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                foreach (var param in Parameters)
                {
                    param.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                foreach (var statement in Statements)
                {
                    statement.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
            }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                base.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                symbols.Add(Helper.MethodNodeToMethodSymbol(this));
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Method);
                }
                ReturnTypeNode.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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
                    $"Phương thức khởi tạo: ({MethodAttributes}) {isResolved}({parameters})",
                    [.. Statements]
                    );
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                base.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                symbols.Add(Helper.MethodNodeToMethodSymbol(this));
                if (collectSpans)
                {
                    // Maybe change it later
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Method);
                }
            }
        }

        public class VariableNode : ASTNode // Used for both fields and parameters
        {
            public required SymbolKind SymbolKind { get; set; }
            public required string Name { get; set; }
            public TypeNode? TypeNode { get; set; }
            public override string ToString()
            {
                return $"{Name} ({TypeNode})";
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                // The symbol is collected by its respective declaration statement

                // So we only collect the spans here if needed
                if (collectSpans)
                {
                    Helper.AddSpan(
                        document,
                        symbolSpans,
                        StartLine,
                        StartColumn,
                        EndLine,
                        EndColumn,
                        SymbolKind); // symbol name
                }
                TypeNode!.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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
            public abstract Type? GetCLRType();
            public abstract void SetCLRType(Type type);
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
            public override Type? GetCLRType()
            {
                return CLRType;
            }
            public override void SetCLRType(Type type)
            {
                CLRType = type;
            }
            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                symbols.Add(Helper.TypeNodeToTypeSymbol(this));
                if (collectSpans)
                {
                    // TODO: Add more types later
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Class);
                }
            }
        }

        public class GenericType : TypeNode
        {
            public Type? CLRType { get; set; }
            public required string OpenGenericName { get; set; }
            public required TypeNode[] TypeArguments { get; set; }
            public override string ToString()
            {
                var isResolved = CLRType == null ? "(*)" : "";
                return $"{FullName}{isResolved}";
            }
            public override Type? GetCLRType()
            {
                return CLRType;
            }
            public override void SetCLRType(Type type)
            {
                CLRType = type;
            }
            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                symbols.Add(Helper.TypeNodeToTypeSymbol(this));
                foreach(var typeArgument in TypeArguments)
                {
                    typeArgument.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Class);
                }
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
            public override Type? GetCLRType()
            {
                return ElementTypeNode.GetCLRType()!.MakeArrayType();
            }
            public override void SetCLRType(Type type)
            {
                ElementTypeNode.SetCLRType(type.GetElementType()!);
            }
            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                ElementTypeNode.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Condition.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach (var statement in IfClause)
                {
                    statement.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                foreach (var statement in ElseClause)
                {
                    statement.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
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
            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Condition.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach(var statement in Body)
                {
                    statement.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
            }
        }

        public class BreakStatement : StatementNode
        {
            public Label? EndLabel { get; set; }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                return;
            }
        }

        public class ReturnStatement : StatementNode
        {
            public ExpressionNode? Expression { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString($"Trả về:", [Expression]);
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Expression?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                return;
            }
        }

        public abstract class DeclarationStatement : StatementNode
        {
            public required VariableNode Variable { set; get; }
            public ExpressionNode? InitialValue { set; get; }
        }

        public class LocalDeclarationStatement : DeclarationStatement
        {
            public MethodBase? DeclaringMethod { get; set; } // Gets resolved after static analysis
            public LocalBuilder? LocalBuilder { set; get; }
            public override string ToString()
            {
                if (InitialValue != null)
                {
                    return Helper.GetTreeString($"Khai báo biến: {Variable} gán", [InitialValue]);
                }
                return $"Khai báo biến: {Variable}";
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                if (collectPrivates)
                {
                    symbols.Add(new LocalSymbol
                    {
                        Method = Helper.MethodNodeToMethodSymbol(DeclaringMethod!),
                        Name = Variable.Name
                    });
                }
                Variable.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                InitialValue?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
            }
        }

        public class FieldDeclarationStatement : DeclarationStatement
        {
            public required NamedType DeclaringTypeNode { get; set; }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                if (FieldAttributes.HasFlag(FieldAttributes.Public) || collectPrivates)
                {
                    symbols.Add(new FieldSymbol {
                        DeclaringType = Helper.TypeNodeToTypeSymbol(DeclaringTypeNode),
                        Name = Variable.Name
                    });
                }
                Variable.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                InitialValue?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Destination.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                Source.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
            }
        }

        public class ExpressionStatement : StatementNode
        {
            public required ExpressionNode Expression { get; set; }
            public override string ToString()
            {
                return $"Biểu thức: {Expression}";
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Expression.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Left.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                Right.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
            }
        }

        public class ThisExpression : ExpressionNode
        {
            public override string ToString()
            {
                return "{này}";
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                return;
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                return;
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Local);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Parameter);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                foreach(var argument in Arguments)
                {
                    argument.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Method);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                base.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                TypeNode.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                base.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                Object.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
            }
        }

        public class BoxExpression : ExpressionNode
        {
            public required ExpressionNode Expression { get; set; }
            public override string ToString()
            {
                return Helper.GetTreeString("Box biểu thức", [Expression]);
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Expression.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                ReturnTypeNode?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                Size?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach(var element in Elements)
                {
                    element.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Array.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                Index.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                ReturnTypeNode?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach(var init in FieldInitializations)
                {
                    init.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                ReturnTypeNode?.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                foreach(var arg in Arguments)
                {
                    arg.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                Object.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Field);
                }
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

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                TypeNode.CollectSymbols(document, symbols, symbolSpans, collectPrivates, collectSpans);
                if (collectSpans)
                {
                    Helper.AddSpan(document, symbolSpans, StartLine, StartColumn, EndLine, EndColumn, SymbolKind.Field);
                }
            }
        }

        public class DuplicateExpression : ExpressionNode
        {
            public override string ToString()
            {
                return "Sao chép đối tượng";
            }

            internal override void CollectSymbols(TextDocument document, HashSet<Symbol> symbols, List<SymbolSpan> symbolSpans, bool collectPrivates, bool collectSpans)
            {
                return;
            }
        }
    }
}