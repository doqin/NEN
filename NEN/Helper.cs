using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Document;
using NEN.AST;
using NEN.Symbols;
using System.Collections.Immutable;

namespace NEN
{
    internal class Helper
    {
        internal static string GetTreeString<T>(string? head, T[] nodes)
        {
            var str = string.IsNullOrEmpty(head) ? "" : $"{head}\n";
            foreach (var (n, i) in nodes.Select((c, i) => (c, i)))
            {
                str += string.IsNullOrEmpty(head) && i == 0 ? "" : $"|\n";
                var strLines = (n?.ToString() ?? "").Split(['\r', '\n'], StringSplitOptions.RemoveEmptyEntries);
                foreach (var (line, j) in strLines.Select((line, j) => (line, j)))
                {
                    if (j == 0)
                    {
                        str += $"+- {line}\n";
                    }
                    else if (i < nodes.Length - 1)
                    {
                        str += $"|  {line}\n";
                    }
                    else
                    {
                        str += $"   {line}\n";
                    }
                }
            }
            return str;
        }

        internal static void AddSpan(
            TextDocument document, 
            List<SymbolSpan> spans, 
            int startLine, 
            int startColumn, 
            int endLine, 
            int endColumn, 
            SymbolKind kind
        )
        {
            if (startLine < 1 || startColumn < 1 || endLine < 1 || endColumn < 1) return;
            try
            {
                var startOffset = GetOffset(document, startLine, startColumn);
                var endOffset = GetOffset(document, endLine, endColumn + 1);
                endOffset = Math.Min(endOffset, document.TextLength);
                if (endOffset > startOffset)
                {
                    spans.Add(new SymbolSpan(startOffset, endOffset, kind));
                }
            }
            catch
            {
                // Ignore invalid spans
            }
        }

        internal static int GetOffset(TextDocument document, int line, int column)
        {
            var documentLine = document.GetLineByNumber(line);
            var cappedColumn = Math.Clamp(column - 1, 0, documentLine.Length);
            return documentLine.Offset + cappedColumn;
        }

        internal static Symbols.MethodBase MethodNodeToMethodSymbol(AST.MethodBase methodBase)
        {
            return methodBase switch
            {
                MethodNode methodNode => new MethodSymbol
                {
                    Name = methodNode.MethodName,
                    DeclaringType = TypeNodeToTypeSymbol(methodNode.DeclaringTypeNode),
                    Parameters = [.. methodNode.Parameters.Select(p => TypeNodeToTypeSymbol(p.TypeNode!))]
                },
                ConstructorNode constructorNode => new ConstructorSymbol
                {
                    Name = constructorNode.DeclaringTypeNode.FullName,
                    DeclaringType = TypeNodeToTypeSymbol(constructorNode.DeclaringTypeNode),
                    Parameters = [.. constructorNode.Parameters.Select(p => TypeNodeToTypeSymbol(p.TypeNode!))]
                },
                _ => throw new NotImplementedException(),
            };
        }

        internal static Symbols.TypeSymbol TypeNodeToTypeSymbol(AST.TypeNode typeNode)
        {
            if (typeNode.GetCLRType()!.IsClass)
            {
                return new ClassSymbol { Name = typeNode.FullName };
            }
            else if (typeNode.GetCLRType()!.IsAnsiClass)
            {
                return new ANSISymbol { Name = typeNode.FullName };
            }
            else
            {
                throw new NotImplementedException();
            }
        }
    }
}
